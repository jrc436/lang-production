package edu.psu.acs.lang.eval.target;

import java.io.IOException;

public class CompareMain {
	public static void main(String[] args) throws IOException {
		Rouge r = new Rouge();
		System.out.println(Comparer.evaluateAllAvg(r::n));
		System.out.println(Comparer.evaluateAllAvg(r::l));
		System.out.println(Comparer.evaluateAllAvg(r::s));
	}
}
