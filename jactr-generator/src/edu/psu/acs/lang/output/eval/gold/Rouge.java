package edu.psu.acs.lang.output.eval.gold;

import java.util.List;

import edu.psu.acs.lang.output.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.eval.core.Evaluator;
import edu.psu.acs.lang.output.eval.ngram.GramSet;
import edu.psu.acs.lang.output.eval.ngram.Grammifier;
import edu.psu.acs.lang.output.eval.ngram.LCSequencer;
import edu.psu.acs.lang.output.eval.ngram.LeastCommonSubsequence;
import edu.psu.acs.lang.output.eval.ngram.NgramMaker;
import edu.psu.acs.lang.output.eval.ngram.Segmenter;
import edu.psu.acs.lang.output.eval.ngram.SimpleGrammifier;
import edu.psu.acs.lang.output.eval.ngram.SimpleSequencer;
import edu.psu.acs.lang.output.eval.ngram.SimpleSkipBigrams;
import edu.psu.acs.lang.output.eval.ngram.SkipgramMaker;
import edu.psu.acs.lang.output.eval.ngram.Tokenizer;

public class Rouge implements Evaluator<GarbleGold> {
	private static final double defaultBeta = 0.5;
	private final Tokenizer token;
	private final LCSequencer lcs;
	private final NgramMaker gram;
	private final SkipgramMaker sg;
	private final double beta;
	public Rouge(Tokenizer t, NgramMaker g, SkipgramMaker sg, double beta, LCSequencer lcs) {
		this.token = t;
		this.gram = g;
		this.sg = sg;
		this.beta = beta;
		this.lcs = lcs;
	}
	public Rouge(Tokenizer t, Grammifier g, int ngramOrder) {
		this(t, NgramMaker.make(g, ngramOrder), new SimpleSkipBigrams(), defaultBeta, new SimpleSequencer(t));
	}
	public Rouge(Tokenizer t, Grammifier skipGrammifier, int skipLength, double beta) {
		this(t, new SimpleGrammifier(1), SkipgramMaker.make(SimpleSkipBigrams::skipBiGrams, 2), beta, new SimpleSequencer(t));
	}
	public Rouge(Tokenizer t, LeastCommonSubsequence lcs, Segmenter s) {
		this(t, new SimpleGrammifier(1), new SimpleSkipBigrams(), defaultBeta, LCSequencer.makeLCS(lcs, s, t));
	}
	public Rouge(Tokenizer t) {
		this(t, new SimpleGrammifier(1), new SimpleSkipBigrams(), defaultBeta, new SimpleSequencer(t));
	}
	public Rouge() {
		this(Tokenizer::simpleTokenize);
		
	}
	public double n(String realization, String goal) {
		GramSet gsr = new GramSet(gram, token, gram.grammify(token.tokenize(realization)));
		GramSet gsg = new GramSet(gram, token, gram.grammify(token.tokenize(goal)));
		return ((double) gsg.setIntersect(gsr).size()) / ((double) gsg.size());
	}
	/**
	 * Important note, this treats all text as a *single* sentence unless your LCS function preprocesses somehow
	 * @param realization
	 * @param goal
	 * @return
	 */
	public double l(String realization, String goal) {
		List<Integer> lcsAcc = lcs.lcsAcc(realization, goal);
		int sum = 0;
		for (Integer i : lcsAcc) {
			sum+= i;
		}
		double lcsr = ((double) sum) / ((double) token.tokenize(realization).size());
		double lcsp = ((double) sum) / ((double) token.tokenize(goal).size());
		double retval = f(lcsp, lcsr);
		if (Double.isNaN(retval)) {
			System.err.println("lol!");
		}
		return retval;
	}
	public double s(String realization, String goal) {
		GramSet gsr = new GramSet(sg, token, sg.grammify(token.tokenize(realization)));
		GramSet gsg = new GramSet(sg, token, sg.grammify(token.tokenize(goal)));
		int skip = gsg.setIntersect(gsr).size();
		if (skip == 0) {
			return 0;
		}
		double skipr = ((double) skip) / ((double) gsg.size());
		double skipp = ((double) skip) / ((double) gsr.size());
		return f(skipp, skipr);
	}
	
	private double f(double p, double r) {
		if (r == 0 && p == 0) {
			return 0;
		}
		if (0 <= beta && beta <= 1) {
			return ((1 + beta * beta) * r * p) / (r + beta * beta * p);
		}
		return r;
	}
	@Override
	public String evalName() {
		return "rouge-l,rouge-n,rouge-s";
	}
	@Override
	public String evaluate(GarbleGold data) {
		List<String> frags = data.getGarble().getFragments();
		double savg = 0.0;
		for (String frag : frags) {
			savg += s(frag, data.getGold());
		}
		savg /= frags.size();
		
		double lavg = 0.0;
		for (String frag : frags) {
			lavg += l(frag, data.getGold());
		}
		lavg /= frags.size();
		
		double navg = 0.0;
		for (String frag : frags) {
			navg += n(frag, data.getGold());
		}
		navg /= frags.size();
		return String.format("%.3f,%.3f,%.3f",lavg,navg,savg);
	}
}
