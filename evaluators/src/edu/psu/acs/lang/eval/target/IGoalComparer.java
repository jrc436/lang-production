package edu.psu.acs.lang.eval.target;

@FunctionalInterface
public interface IGoalComparer {
	public double score(String realization, String goal);
}
