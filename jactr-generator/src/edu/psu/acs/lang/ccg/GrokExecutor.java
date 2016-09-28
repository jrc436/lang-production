package edu.psu.acs.lang.ccg;

import util.sys.Executor;

public class GrokExecutor extends Executor<CCGParseListGrokker, CCGParseList, CCGParseList> {

	public GrokExecutor() {
		super("grokccg", 12, CCGParseListGrokker.class, CCGParseList.class, CCGParseList.class);
		// TODO Auto-generated constructor stub
	}
	public static void main(String[] args) {
		GrokExecutor fe = new GrokExecutor();
		fe.initializeFromCmdLine(args);
		fe.run();
	}

}
