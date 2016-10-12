package edu.psu.acs.lang.eval.ngram;

import java.util.List;

@FunctionalInterface
public interface Grammifier {
	/**
	 * Transforms tokens into ngrams. 
	 * @param tokens
	 * A list of tokens, created by some Tokenizer
	 * @param order
	 * The order the ngrams should be of
	 * @return
	 * a list of ngrams of the specified order
	 */
	public List<String> grammify(List<String> tokens, int order);
}
