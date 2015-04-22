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
	public int hashCode() {
		return (int) (Math.pow(47, 1) * completeness) + (int) (Math.pow(47, 2) * avgScore) + (int) (Math.pow(47,  3) * avgCompleteScore); 
	}
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) { return false; }
		Evaluation e = (Evaluation) o;
		return e.completeness == this.completeness && this.avgScore == e.avgScore && this.avgCompleteScore == e.avgCompleteScore;
	}
	public String toString() {
		return "score: "+String.format("%.5f", this.getScore())+"; "+"completeness: "+String.format("%.5f", this.completeness)+";";
	}
}
