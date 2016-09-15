package edu.psu.acs.lang.eval.ngram;

public interface UnaryEvaluator {
	public double score(String s);
	public static double convertToProb(double d) {
		return Math.pow(10, d);
	}
}
