package edu.psu.acs.lang.eval.data.garblegold;

import edu.psu.acs.lang.output.tree.GarbleTreeList;
import util.sys.Executor;

public class OutputToGarbleExecutor extends Executor<OutputToGarbleProcessor, GarbleTreeList, GarbleTreeList> {

	public OutputToGarbleExecutor() {
		super("garbleout", 4, OutputToGarbleProcessor.class, GarbleTreeList.class, GarbleTreeList.class);
	}
	public static void main(String[] args) {
		OutputToGarbleExecutor tle = new OutputToGarbleExecutor();
		tle.initializeFromCmdLine(args);
		tle.run();
	}

}
