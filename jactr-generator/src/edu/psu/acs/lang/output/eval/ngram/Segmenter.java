package edu.psu.acs.lang.output.eval.ngram;

import java.util.Arrays;
import java.util.List;

public interface Segmenter {
	public List<String> segment(String sent);
	public static List<String> noSegment(String sent) {
		return Arrays.asList(sent);
	}
}
