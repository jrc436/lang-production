package edu.psu.acs.lang.ccg;

import util.sys.Executor;

public class FilterExecutor extends Executor<CCGParseListFilter, CCGParseList, CCGParseList> {

	public FilterExecutor() {
		super("filterccg", 12, CCGParseListFilter.class, CCGParseList.class, CCGParseList.class);
	}
	public static void main(String[] args) {
		FilterExecutor fe = new FilterExecutor();
		fe.initializeFromCmdLine(args);
		fe.run();
	}

}
