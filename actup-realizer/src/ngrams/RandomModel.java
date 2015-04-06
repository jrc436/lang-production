package ngrams;

import java.util.HashMap;
import java.util.Random;

import lexicon.Tokenizer;
import synsem.Sign;

public class RandomModel extends AbstractStandardNgramModel {

	HashMap<Sign, Double> signScores;
	Random gen;
	public RandomModel(int order, Tokenizer tokenizer) {
		super(order, tokenizer);
		gen = new Random();
		signScores = new HashMap<Sign, Double>();
	}
	public synchronized double score(Sign sign, boolean complete) {
		if (signScores.containsKey(sign)) { 
			return signScores.get(sign);
		}
		signScores.put(sign, gen.nextDouble());
		return signScores.get(sign);
	}

}
