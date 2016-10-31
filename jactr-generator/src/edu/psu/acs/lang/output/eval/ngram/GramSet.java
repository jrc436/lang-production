package edu.psu.acs.lang.output.eval.ngram;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class GramSet extends HashSet<String> {
	private static final long serialVersionUID = -8338694341574055200L;
	private final Tokenizer t;
	private final Grammifier g;
	public GramSet(List<String> sentences) {
		this();
		addSentences(sentences);
	}
	public GramSet() {
		super();
		this.t = Tokenizer::simpleTokenize;
		this.g = SimpleGrammifier::sgrammify;
	}
	public GramSet(Grammifier g, Tokenizer t) {
		super();
		this.t = t;
		this.g = g;
	}
	public GramSet(Grammifier g, Tokenizer t, List<String> gramsToAdd) {
		this(g, t);
		addGrams(gramsToAdd);
	}
	public void addGrams(List<String> grams) {
		this.addAll(grams);
	}
	public void addGrams(GramSet grams) {
		this.addAll(grams);
	}
	public void addSentences(List<String> sentences) {
		for (String s : sentences) {
			List<String> words = g.grammify(t.tokenize(s), 1);
			for (String w : words) {
				w = w.toLowerCase().trim();
				if (!w.isEmpty()) {
					super.add(w);
				}
			}
		}
	}
	public boolean isSuperSet(GramSet ws) {
		boolean containsAll = true;
		for (String s : ws) {
			containsAll = containsAll && this.contains(s);
		}
		return containsAll;
	}
	public GramSet setDiff(GramSet ws) {
		List<String> words = new ArrayList<String>();
		for (String s : ws) {
			if (!this.contains(s)) {
				words.add(s);
			}
		}
		return new GramSet(words);
	}
	public GramSet setIntersect(GramSet ws) {
		List<String> sharedGrams = new ArrayList<String>();
		for (String s : this) {
			if (ws.contains(s)) {
				sharedGrams.add(s);
			}
		}
		return new GramSet(this.g, this.t, sharedGrams);
	}
	public boolean isSubSet(GramSet ws) {
		boolean containsAll = true;
		for (String s : this) {
			containsAll = containsAll && ws.contains(s);
		}
		return containsAll;
	}
}
