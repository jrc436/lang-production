package edu.psu.acs.lang.lexsyn;

import edu.psu.acs.lang.ccg.CCGParseList;
import util.sys.Executor;

public class WordInfoExecutor extends Executor<WordInfoProcessor, CCGParseList, LexsynOrderedList> {

	public WordInfoExecutor() {
		super("words", 5, WordInfoProcessor.class, CCGParseList.class, LexsynOrderedList.class);
	}
	public static void main(String[] args) {
		WordInfoExecutor wie = new WordInfoExecutor();
		wie.initializeFromCmdLine(args);
		wie.run();
	}

}
