package optimization;

import runconfig.ThreadState;


public interface Optimizer {
	public VariableSet optimizeVariables(String experimentName, VariableSet opt, int maxIter, ThreadState ts);
}
