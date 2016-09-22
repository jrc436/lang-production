package edu.psu.acs.lang.gen;

import edu.psu.acs.lang.lexsyn.CCGParseList;
import util.sys.Executor;

public class SWBDSplitExecutor extends Executor<SWBDSplitProcessor, CCGParseList, CCGParseList> {
	public SWBDSplitExecutor() {
		super("split", 40, SWBDSplitProcessor.class, CCGParseList.class, CCGParseList.class);
	}
	public static void main(String[] args) {
		SWBDSplitExecutor split = new SWBDSplitExecutor();
		split.initializeFromCmdLine(args);
		split.run();
	}

}
