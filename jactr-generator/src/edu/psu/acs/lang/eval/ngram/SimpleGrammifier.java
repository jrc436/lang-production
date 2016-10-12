package edu.psu.acs.lang.eval.ngram;

import java.util.ArrayList;
import java.util.List;

public class SimpleGrammifier extends NgramMaker {
	public SimpleGrammifier(int order) {
		super(order);
	}
	public List<String> grammify(List<String> tokens, int order) {
		return sgrammify(tokens, order);
	}
	public static List<String> sgrammify(List<String> tokens, int order) {
		List<String> ngrams = new ArrayList<String>();
		for (int i = 0; i < tokens.size(); i++) {
			if (i + order < tokens.size()) {
				String gram = "";
				for (int j = i; j < i + order; j++) {
					gram += tokens.get(j) + " ";
				}
				ngrams.add(gram.substring(0, gram.length()-1));
			}
		}
		return ngrams;
	}
}
