package edu.psu.acs.lang.eval.ngram;

import java.util.List;

public abstract class SkipgramMaker extends NgramMaker {
	public SkipgramMaker(int order) {
		super(order);
	}
	public static SkipgramMaker make(Grammifier g, int order) {
		return new SkipgramMaker(order) {
			public List<String> grammify(List<String> tokens, int order) {
				return g.grammify(tokens, order);
			}
		};
	}
}
