package optimization;

import java.io.IOException;

public interface Optimizer {
	public VariableSet optimizeVariables(String experimentName, VariableSet opt, int maxIter) throws IOException;
}
