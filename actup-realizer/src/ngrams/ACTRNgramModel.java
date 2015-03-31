package ngrams;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import lexicon.Word;
import synsem.Sign;



public class ACTRNgramModel extends StandardNgramModel {
	public static final int negD_index = 0;
	public static final int ey_index = 1; 
	public static final int psp_index = 2; 
	public static final int k_index = 3;
	
	private static final double speaking_rate = 196.0; //cite swbd speaking rate paper
	private static final double words_per_ngram = 1.0; //pulled from thin air, but I believe this should always be 1.0, see speaking_rate_eff
	private static final double year_seconds = 31557600; //365.25 * 24 * 3600	
		
	//the first division is to convert it into words / second. The second division is to convert it into ngrams / second. 
	//Realistically, as we're really using this to compute the TIME that's passing, NOT the ngrams, words_per_ngram shoudl always be 1.0
	private static final double speaking_rate_eff = (speaking_rate / 60.0) / words_per_ngram;
	
	private double priorExposureTime; //this is assignable because the params can change!
	private double convoElapsedTime = 0.0;

	public double exposureTime() { 
		return convoElapsedTime + priorExposureTime;
	}
	
	//the strings are the ngrams, the doubles are the most recent k presentation timestamps.
	//the most recent timestamp is always stored at 0, the least recent at k
	private Presentations nGramPresentations;
	
	public ACTRNgramModel(double[] varValues, int order, String modelFile) throws IOException {
		super(order, modelFile, varValues);
		this.priorExposureTime = year_seconds * params[ey_index];
		nGramPresentations = new Presentations((int) Math.round(params[k_index]));
	}
	public synchronized double score(Sign sign, boolean complete) {
		List<Word> s = sign.getWords();
		String sf = "";
		for (Word w : s) {
			sf += w.getForm() + " ";
		}
		sf = sf.substring(0, sf.length()-1);
		//This is exactly the formula for the ACT-R n-gram formula
		double prior = super.score(sign, complete);
		double activation = getActivation(sf, prior);
		double actProbRatio = Math.exp(activation);
		double actProb = actProbRatio / (actProbRatio + 1.0);
		//activation = Math.exp(activation) - 1;
    	return actProb;//activation;
	}
	public void clean() {
		nGramPresentations = new Presentations((int) Math.round(params[k_index]));
		this.priorExposureTime = year_seconds * params[ey_index];
	}
	private HashSet<String> getNgramsFromSentence(String sentence) {
		HashSet<String> ngrams = new HashSet<String>();
		String[] words = sentence.split(" ");
		for (int i = 1; i <= words.length; i++) {
			for (int j = 0; j < words.length-i; j++) {
				String ngram = "";
				for (int k = j; k < j+i && k<super.order; k++) {
					ngram += words[k]+" ";
				}
				if (ngram != "") {
					ngrams.add(ngram.substring(0, ngram.length()-1));
				}
			}
		}
		return ngrams;
	}
	/**
	 * update the activation table
	 */
	public synchronized void updateAfterRealization(String sentence) {
		// first need to parse the sentence into n-grams
		HashSet<String> ngrams = getNgramsFromSentence(sentence);
		double time_approximate = ((double) sentence.split(" ").length) / speaking_rate_eff;
		convoElapsedTime += time_approximate;
		for (String s : ngrams) {
			nGramPresentations.updateContext(s);
		}
		nGramPresentations.decayPresentations(time_approximate);
	}
	
	public synchronized double getActivation(String sf, double ngramPrior) {
		return nGramPresentations.getActivation(sf, ngramPrior, this.exposureTime());
	}
	
	class Presentations extends HashMap<String, Chunk> {		
		private final int depth;
		public HashSet<String> uninitialized;
		
		public Presentations(int k) {
			this.depth = k;
			uninitialized = new HashSet<String>();
		}
		
		
		
		//the ngramPrior and totalTime are just to init the chunk if necessary
		//t_n = total_lifetime
		//t_k = lifetime of 1 to k
		//n = total presentations
		//k = depth
		//sum of times from i = 1 to k (instead of n) + (n - k)(t_n^(1-d) - t_k^(1-d)) / [ (1-d)(t_n - t_k) ]
		public double getActivation(String sf, double ngramPrior, double totalTime) {
			initChunkIfNeeded(sf, ngramPrior, totalTime);
			Chunk c = super.get(sf);
			double t_n = c.t_n();
			double t_k = c.t_k();
			double n = c.getTotalPresentations();
			double k = (double) this.depth;
			double dc = 1.0 + params[negD_index];
			double check =  c.calculateKActivation() + ( (n-k) * (Math.pow(t_n, dc) - Math.pow(t_k, dc)) ) / ( dc * (t_n - t_k) );
			if (check <= 0.0 || (check + 1.0) <= .0000001) {
				System.err.println("Houston we have a problem.");
			}
			return Math.log(check);
		}
		
		private void initChunkIfNeeded(String chunk, double ngramPrior, double totalTime) {
			if (!super.containsKey(chunk)) {
				this.initChunk(chunk, ngramPrior, totalTime);
			}
			else if (uninitialized.contains(chunk)) {
				forceInitChunk(super.get(chunk), ngramPrior, totalTime);
				uninitialized.remove(chunk);
			}
		}
		
		//exposure_seconds / N is one "interval". assume the first occurrence of that ngram (corresponding to t_n) is t_0 + "interval"
		//assume the last occurrences of that ngram, are t_current - i * interval
		private void initChunk(String chunk, double ngramPrior, double totalTime) {
			int numPresentations = calculateNumPresentations(ngramPrior);
			super.put(chunk, new Chunk(depth, numPresentations, calculateInitTime(numPresentations, totalTime)));		
			//populate k most recent exposures
			Chunk c = super.get(chunk);
			addBaseRecency(c, totalTime, numPresentations);
		}
		private void addBaseRecency(Chunk c, double totalTime, int numPresentations) {
			double N = (double) numPresentations;
			double interval = priorExposureTime / (N+2); //add two, because we're starting at one interval from t0 and ending one interval before the convo
			for (int i = 0; i < depth; i++) {
				double j = (double) (i+1);
				c.addPresentation((totalTime - priorExposureTime) + j * interval, false); //these presentations were already added to the numPresentations, so don't need to be added again
			}
		}
		private int calculateNumPresentations(double ngramPrior) {
			//subtracting k because those will be added when we add them as baserecency
			int numPresentations = (int) Math.round(priorExposureTime * ngramPrior * speaking_rate_eff * params[psp_index]);
			return Math.max(1, Math.max(depth, numPresentations)); //need at least depth presentations for it to really make sense...
																   //need at least one presentation for the math to work at all!
		}
		private double calculateInitTime(int numPresentations, double totalTime) {
			double N = (double) numPresentations;
			double interval = priorExposureTime / (N+2); //see addbaserecency
			return totalTime - interval; //one interval "after" totaltime.
		}
		
		
		private void forceInitChunk(Chunk c, double ngramPrior, double totalTime) {
			int basePres = calculateNumPresentations(ngramPrior);
			double init = calculateInitTime(basePres, totalTime);
			c.addInitInfo(basePres, init);
			addBaseRecency(c, totalTime, basePres);
		}
		
		//force adds any chunks that are not presently already in the thing
		//adds the new presentation of that ngram
		public void updateContext(String ngram) {
			if (!super.containsKey(ngram)) {
				this.forceAddChunk(ngram);
			}
			super.get(ngram).addPresentation();
		}
		
		//this is when no nGramPrior is available. It still needs to be initialized when possible
		private void forceAddChunk(String chunk) {
			Chunk c = new Chunk(depth, 0, 0);
			super.put(chunk, c); //this is all we can do without the ngram info
			uninitialized.add(chunk);
		}
		
		
		public void decayPresentations(double time) {
			for (Chunk c : super.values()) {
				c.decay(time);
			}
		}
		
		
		
		private static final long serialVersionUID = 8099792661937011374L;
		
	}
	class Chunk {
		private final double[] recentP;
		private int totalP;
		double initialTime;
		public Chunk(int k, int totalP, double initialTime) {
			recentP = new double[k];
			for (int i = 0; i < recentP.length; i++) {
				recentP[i] = Double.MAX_VALUE;
			}
			this.totalP = totalP;
			this.initialTime = initialTime;
		}
		public int getTotalPresentations() {
			return totalP;
		}
		
		public void addInitInfo(int totalP, double initialTime) {
			this.totalP += totalP; //don't want to override the ones already there!
			this.initialTime = initialTime; 
		}
		
		public double t_k() {
			if (recentP.length == 0) {
				return 0;
			}
			return recentP[recentP.length-1]; //returns the least recent still tracked timestamp as desired by the equation
		}
		
		//this time refers to how many seconds it's been in existence. 
		//since all times are in the frame of "how many seconds ago", initialTime is good enough 
		public double t_n() {
			return initialTime;
		}
		public double calculateKActivation() {
			double activation = 0.0;
			for (double time : recentP) {
				activation += Math.pow(time, params[negD_index]);
			}
			return activation;
		}
		
		public void addPresentation() {
			addPresentation(0.0, true);
		}
		
		public void addPresentation(double t, boolean counts) {
			for (int i = 0; i < recentP.length; i++) {
				//times here refer to "how many seconds ago"
				if (recentP[i] > t) {
					//insert and shift.
					double toInsert = t;
					for (int j = i; j < recentP.length; j++) {
						double tmp = recentP[j];
						recentP[j] = toInsert;
						toInsert = tmp; 
					}
					//at the end, toInsert should be the element that gets "bumped"
					break;
				}
			}
			if (counts) {
				totalP++;
			}
		}
		
		public void decay(double time) {
			for (int i = 0; i < recentP.length; i++) {
				recentP[i] += time;
			}
			initialTime += time;
		}
	}
}
