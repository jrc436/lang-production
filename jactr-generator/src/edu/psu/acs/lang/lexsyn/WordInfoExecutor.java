package edu.psu.acs.lang.lexsyn;

import util.sys.Executor;
import util.wordmap.WordMap;

public class WordInfoExecutor extends Executor<WordInfoProcessor, CCGParseList, WordMap> {

	public WordInfoExecutor() {
		super("words", 20, WordInfoProcessor.class, CCGParseList.class, WordMap.class);
	}
	public static void main(String[] args) {
		WordInfoExecutor wie = new WordInfoExecutor();
		wie.initializeFromCmdLine(args);
		wie.run();
	}

}
