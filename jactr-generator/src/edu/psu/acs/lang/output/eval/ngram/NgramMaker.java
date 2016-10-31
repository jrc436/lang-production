package edu.psu.acs.lang.output.eval.ngram;

import java.util.List;

public abstract class NgramMaker implements Grammifier {
	private final int order;
	public NgramMaker(int order) {
		this.order = order;
	}
	public static NgramMaker make(Grammifier g, int order) {
		return new NgramMaker(order) {
			public List<String> grammify(List<String> tokens, int order) {
				return g.grammify(tokens, order);
			}
		};
	}
	public List<String> grammify(List<String> tokens) {
		return this.grammify(tokens, order);
	}
}
