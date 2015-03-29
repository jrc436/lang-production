package opennlp.ccg.optimization;

public interface Optimizer {
	public Variable[] optimizeVariables(String experimentName, int maxIter, int startIndex);
}
