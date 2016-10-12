package edu.psu.acs.lang.sentences;

import edu.psu.acs.lang.ccg.CCGParseList;
import util.sys.Executor;

public class SentenceListExecutor extends Executor<SentenceListCreator, CCGParseList, SentenceList> {
	public SentenceListExecutor() {
		super("sentences", 4, SentenceListCreator.class, CCGParseList.class, SentenceList.class);
	}
	public static void main(String[] args) {
		SentenceListExecutor tle = new SentenceListExecutor();
		tle.initializeFromCmdLine(args);
		tle.run();
	}

}
