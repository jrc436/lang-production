package evaluation;

import java.util.Arrays;
import java.util.List;

import runconfig.ScoringStrategy;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

//more or less a set of scores and some methods on them.
public class Evaluation {
	private final double completeness;
	private final double avgScore;
	private final double avgCompleteScore;
	private final ScoringStrategy strat;
	private final double complStdDev; //only completes
	private final double totStdDev; //totals
	private final List<Score> scores;
	public Evaluation(ScoringStrategy ss, Score[] scores) {
		this(ss, Arrays.asList(scores));
	}
	public Evaluation(ScoringStrategy ss, List<Score> scores) {
		double completeNum = 0.0;
		double totalNum = 0.0;
		double totalScore = 0.0;
		double completeScore = 0.0;
		for (Score score : scores) {
			totalScore += score.score;
			totalNum++;
			if (score.complete) {
				completeNum++;
				completeScore += score.score;
			}
		}
		double totScore = totalScore / totalNum;
		double complScore = completeScore / completeNum;
		double totstddev = 0.0;
		double complstddev = 0.0;
		for (Score score : scores) {
			totstddev += Math.pow((score.score - totScore), 2);
			if (score.complete) {
				complstddev += Math.pow((score.score - complScore), 2);
			}
		}
		totstddev /= totalNum;
		complstddev /= completeNum;
		complstddev = Math.sqrt(complstddev);
		totstddev = Math.sqrt(totstddev);
		this.avgScore = totScore;
		this.avgCompleteScore = complScore;
		this.totStdDev = totstddev;
		this.complStdDev = complstddev;
		this.strat = ss;
		this.completeness = ((double) completeNum) / ((double) totalNum);
		this.scores = scores;
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
	public String writeScores() {
		String toWrite = "{";
		for (Score s : scores) {
			if (s.complete || strat == ScoringStrategy.ScoreAll || strat == ScoringStrategy.AdjScoreAll) {
				toWrite += String.format("%.5f", s.score)+",";
			}
		}
		return toWrite.substring(0, toWrite.length()-2)+"}";
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
		return "score: "+String.format("%.5f", this.getScore())+"; completeness: "+String.format("%.5f", this.completeness)+"; stddev: "+String.format("%.5f", this.getStdDeviation())+"; scoreList:"+writeScores();
	}
}
