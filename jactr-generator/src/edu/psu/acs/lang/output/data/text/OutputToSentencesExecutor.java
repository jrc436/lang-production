package edu.psu.acs.lang.output.data.text;

import edu.psu.acs.lang.output.data.tree.GarbleTreeList;
import edu.psu.acs.lang.sentences.SentenceList;
import util.sys.Executor;

public class OutputToSentencesExecutor extends Executor<OutputToSentencesProcessor, GarbleTreeList, SentenceList> {
	public OutputToSentencesExecutor() {
		super("outsentence", 4, OutputToSentencesProcessor.class, GarbleTreeList.class, SentenceList.class);
	}
	public static void main(String[] args) {
		OutputToSentencesExecutor tle = new OutputToSentencesExecutor();
		tle.initializeFromCmdLine(args);
		tle.run();
	}

}
