package evaluation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import realize.Realization;
import runconfig.ScoringStrategy;

/**
 * an evaluator loads a set of strings that have been placed in a set of files under a specific folder.
 * The only real requirement is that the files are in "sentence per line" format 
 * @author jrc
 */
public abstract class Evaluator implements TextScorer {
	private ScoringStrategy ss;
	public Evaluator(ScoringStrategy ss) {
		this.ss = ss;
	}
	public Score score(Realization r) {
		return new Score(this.score(r.str, r.goal), r.complete);
	}
	public Evaluation scoreAll(Realization[] realizations) {
		return this.scoreAll(Arrays.asList(realizations));
	}
	public Evaluation scoreAll(List<Realization> realizations) {
		List<Score> scores = new ArrayList<Score>(realizations.size());
		for (Realization r : realizations) {
			scores.add(this.score(r));
		}
		return new Evaluation(ss, scores);
	}
}
