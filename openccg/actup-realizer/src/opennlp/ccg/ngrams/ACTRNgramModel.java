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
	private static final double negD = -0.5; //hindi paper acl
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
		//This is exactly the formula for the ACT-R n-gram formula
		double prior = super.score(sign, complete); 
		double activation = getActivation(sf);
		if (activation == -1) {
			//E[t] = 5 * log(0.5) / log(1 - prior)
			activation = Math.pow(5.0 * Math.log(0.5) / Math.log(1.0 - prior), negD);
		}
		activation = Math.exp(activation) - 1;
    	return prior * activation;
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
	/**
	 * update the activation table
	 */
	public synchronized void updateAfterRealization(String sentence) {
		// first need to parse the sentence into n-grams
		HashSet<Integer> ngrams = getNgramsFromSentence(sentence);
		double time_approximate = ((double) sentence.split(" ").length) / (196.0 / 60.0);//196 wpm
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
		List<Double> presentations = nGramPresentations.get(sf.hashCode());
		if (presentations == null) {
			return -1; //we'll have to calculate a prior
		}
		for (Double t : presentations) {
			sum += Math.pow(t, negD);
		}
		return Math.log(sum);
	}
}
