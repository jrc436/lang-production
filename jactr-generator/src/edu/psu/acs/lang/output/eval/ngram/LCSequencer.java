package edu.psu.acs.lang.output.eval.ngram;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class LCSequencer implements LeastCommonSubsequence {
	private final Segmenter s;
	private final Tokenizer token;
	public LCSequencer(Segmenter s, Tokenizer t) {
		this.s = s;
		this.token = t;
	}
	public LCSequencer(Tokenizer t) {
		this(Segmenter::noSegment, t);
	}
//	public LCSequencer() {
//		this(Tokenizer::simpleTokenize);
//	}
	public List<Integer> lcsAcc(String realized, String goal) {
		List<Integer> lcsAcc = new ArrayList<Integer>();	
		for (String str : s.segment(goal)) {
			List<String> tokens = token.tokenize(str);
			Set<String> lcsUnion = new HashSet<String>();
			for (String f : s.segment(realized)) {
				lcsUnion.addAll(this.lcs(token.tokenize(f), tokens));
			}
			lcsAcc.add(lcsUnion.size());
		}
		return lcsAcc;
	}
	public static LCSequencer makeLCS(LeastCommonSubsequence lcs, Segmenter s, Tokenizer t) {
		return new LCSequencer(s, t) {
			public List<String> lcs(List<String> a, List<String> b) {
				return lcs.lcs(a, b);
			}
		};
	}
}
