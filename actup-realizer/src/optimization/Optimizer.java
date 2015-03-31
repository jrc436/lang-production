package optimization;

public interface Optimizer {
	public VariableSet optimizeVariables(String experimentName, VariableSet opt, int maxIter);
}
