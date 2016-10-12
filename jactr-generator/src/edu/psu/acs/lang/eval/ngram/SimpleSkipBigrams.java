package edu.psu.acs.lang.eval.ngram;

import java.util.ArrayList;
import java.util.List;

public class SimpleSkipBigrams extends SkipgramMaker {
	public SimpleSkipBigrams() {
		super(2);
	}

	public static List<String> skipBiGrams(List<String> tokens, int order) {
		if (order != 2) {
			throw new IllegalArgumentException("only supports bigrams");
		}
		List<String> retval = new ArrayList<String>();
		for (int i = 0; i < tokens.size() - 1; i++) {
			for (int j = i + 1; j < tokens.size(); j++) {
				retval.add(tokens.get(i)+" "+tokens.get(j));
			}
		}
		return retval;
	}
	@Override
	public List<String> grammify(List<String> tokens, int order) {
		if (order != 2) {
			throw new IllegalArgumentException("only supports bigrams");
		}
		return skipBiGrams(tokens, order);
	}
}
