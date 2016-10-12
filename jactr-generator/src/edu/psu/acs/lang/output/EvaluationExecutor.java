package edu.psu.acs.lang.output;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGoldList;
import util.sys.Executor;

public class EvaluationExecutor extends Executor<EvaluationOSProcessor, GarbleGoldList, EvaluationOSSet> {
	public EvaluationExecutor() {
		super("evalexec", 4, EvaluationOSProcessor.class, GarbleGoldList.class, EvaluationOSSet.class);
	}
	public static void main(String[] args) {
		EvaluationExecutor tle = new EvaluationExecutor();
		tle.initializeFromCmdLine(args);
		tle.run();
	}
}
