package evaluation;

import runconfig.ScoringStrategy;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

public class Evaluation {
	private final double completeness;
	private final double avgScore;
	private final double avgCompleteScore;
	private final ScoringStrategy strat;
	private final double complStdDev; //only completes
	private final double totStdDev; //totals
	public Evaluation(double completeness, double score, double complScore, double totstddev, double complstddev, ScoringStrategy strat) {
		this.completeness = completeness;
		this.avgScore = score;
		this.avgCompleteScore = complScore;
		this.strat = strat;
		this.complStdDev = complstddev;
		this.totStdDev = totstddev;
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
	public double getStdDeviation() {
		switch(strat) {
			case ScoreAll:
				return totStdDev;
			case ScoreComplete:
				return complStdDev;
			case AdjScoreComplete:
				throw new NotImplementedException();
			case AdjScoreAll:
				throw new NotImplementedException();
			default:
				return complStdDev;
		}
	}
	
	public double getCompleteness() {
		return completeness;
	}
	public int hashCode() {
		return (int) (Math.pow(47, 1) * completeness) + (int) (Math.pow(47, 2) * avgScore) + (int) (Math.pow(47,  3) * avgCompleteScore) + (int) (Math.pow(47, 4) * totStdDev) + (int) (Math.pow(47, 5) * complStdDev); 
	}
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) { return false; }
		Evaluation e = (Evaluation) o;
		return e.completeness == this.completeness && this.avgScore == e.avgScore && this.avgCompleteScore == e.avgCompleteScore && this.totStdDev == e.totStdDev && this.complStdDev == e.complStdDev;
	}
	public String toString() {
		return "score: "+String.format("%.5f", this.getScore())+"; completeness: "+String.format("%.5f", this.completeness)+"; stddev: "+String.format("%.5f", this.getStdDeviation());
	}
}
