package edu.psu.acs.lang.eval.ngram;

import java.util.List;

@FunctionalInterface
public interface LeastCommonSubsequence {
	public List<String> lcs(List<String> a, List<String> b);
}
