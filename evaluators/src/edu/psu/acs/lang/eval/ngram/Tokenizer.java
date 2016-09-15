package edu.psu.acs.lang.eval.ngram;

import java.util.Arrays;
import java.util.List;

@FunctionalInterface
public interface Tokenizer {
	/**
	 * Turns a string into tokens, for usage with an ngram model
	 * @param s
	 * The string to tokenize
	 * @return
	 * An ordered list of tokens
	 */
	public List<String> tokenize(String s);
	/**
	 * Suitable for when your data is already in Penn Treebank format, for instance. 
	 * @author jrc436
	 *
	 */
	public static List<String> simpleTokenize(String s) {
		return Arrays.asList(s.split(" "));
	}
}
