package edu.psu.acs.lang.output.eval;

public interface Evaluator {
	public double score(String s);
	public static double convertToProb(double d) {
		return Math.pow(10, d);
	}
}
