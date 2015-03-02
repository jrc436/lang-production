package opennlp.ccg.ngrams;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;

import opennlp.ccg.lexicon.Word;
import opennlp.ccg.synsem.Sign;

public class ACTRNgramModel extends StandardNgramModel {
	private static final double negD = -0.5;
	
	//private static final double epsilon = 0.001;
	
	//the integers in the list represent the number of intervening words since that presentation
	//the initial integers are the hashcodes of the string, because java.
	private HashMap<Integer, List<Double>> nGramPresentations;
	
	public ACTRNgramModel(int order, String modelFile) throws IOException {
		super(order, modelFile);
		nGramPresentations = new HashMap<Integer, List<Double>>();
	}
	public synchronized double score(Sign sign, boolean complete) {
		List<Word> s = sign.getWords();
		String sf = "";
		for (Word w : s) {
			sf += w.getForm();
		}
		//This is a Baye's Law argument
		double prior = super.score(sign, complete); //P(ngram)
		double activation = Math.exp(getActivation(sf)); //P(activation | ngram)
		double actPrior = Math.exp(-60.0); //P(activation): we'll use about 1 hour for the base rate since an ngram last appeared
    		if (activation == 1.0) {
    			//since the ngram hasn't appeared yet, P(activation | ngram) = P(activation) and the terms cancel 
			//thought: since "the base rate" should take into account ngrams that have occurred, is it lower? This line of reasoning might require a lot of 
			//unknown probabilistic information for a probably mostly irrelevant gain compared to tuning the prior
    			return prior;
    		}
    		else {
    			return prior * activation / actPrior;
    		}
	}

	private HashSet<Integer> getNgramsFromSentence(String sentence) {
		HashSet<Integer> ngrams = new HashSet<Integer>();
		String[] words = sentence.split(" ");
		for (int i = 1; i <= words.length; i++) {
			for (int j = 0; j < words.length-i; j++) {
				String ngram = "";
				for (int k = j; k < j+i; k++) {
					ngram += words[k];
				}
				ngrams.add(ngram.hashCode());
			}
		}
		return ngrams;
	}
	
	public synchronized void updateActivationTable(String sentence) {
		// first need to parse the sentence into n-grams
		HashSet<Integer> ngrams = getNgramsFromSentence(sentence);
		double time_approximate = ((double) sentence.split(" ").length) / 120.0;//120 wpm
		for (Integer hash : ngrams) {
			if (!nGramPresentations.containsKey(hash)) {
				//this is the first presentation of this ngram
				nGramPresentations.put(hash, new ArrayList<Double>()); 
			}
		}
		for (Entry<Integer, List<Double>> entry : nGramPresentations.entrySet()) {
			for (int i = 0; i < entry.getValue().size(); i++) {
				//this is the ngram decay
				entry.getValue().set(i, entry.getValue().get(i) + time_approximate);
			}
			//this is a new presentation of this ngram that needs to be added
			if (ngrams.contains(entry.getKey())) {
				entry.getValue().add(time_approximate);
			}
		}
	}
//	public synchronized double getActivationAllNgrams(String sentence) {
//		double sum = 0.0;
//		
//	}
	public synchronized double getActivation(String sf) {
		double sum = 0.0;
//		if (sf.split(" ").length <= 3) {
//			System.out.println("###########"+sf+"##########");
//		}
		List<Double> presentations = nGramPresentations.get(sf.hashCode());
		if (presentations == null) {
			return 0.0;
		}
		for (Double t : presentations) {
			sum += Math.pow(t, negD);
		}
		return Math.log(sum);
	}
}
