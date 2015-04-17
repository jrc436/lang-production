package evaluation;

import runconfig.ScoringStrategy;

public class Evaluation {
	private final double completeness;
	private final double avgScore;
	private final double avgCompleteScore;
	private ScoringStrategy strat;
	public Evaluation(double completeness, double score, double complScore, ScoringStrategy strat) {
		this.completeness = completeness;
		this.avgScore = score;
		this.avgCompleteScore = complScore;
		this.strat = strat;
	}
	public double getScore() {
		switch(strat) {
			case ScoreAll:
				return avgScore;
			case ScoreComplete:
				return avgCompleteScore;
			case AdjScoreComplete:
				return (1.0 - completeness) * avgCompleteScore;
			case AdjScoreAll:
				return (1.0 - completeness) * avgScore;
			default:
				return avgScore;
		}
	}
	public double getCompleteness() {
		return completeness;
	}
}
