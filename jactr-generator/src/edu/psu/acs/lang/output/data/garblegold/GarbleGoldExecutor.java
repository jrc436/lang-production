package edu.psu.acs.lang.output.data.garblegold;

import util.sys.Executor;

public class GarbleGoldExecutor extends Executor<GarbleGoldCombiner, GarbleGoldList, GarbleGoldList> {

	public GarbleGoldExecutor() {
		super("ggcombine", 4, GarbleGoldCombiner.class, GarbleGoldList.class, GarbleGoldList.class);
	}
	public static void main(String[] args) {
		GarbleGoldExecutor tle = new GarbleGoldExecutor();
		tle.initializeFromCmdLine(args);
		tle.run();
	}
}
