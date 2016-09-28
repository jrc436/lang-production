package edu.psu.acs.lang.sentences;

import java.io.File;

import edu.psu.acs.lang.ccg.CCGParseList;
import edu.psu.acs.lang.util.ParseException;
import edu.psu.acs.lang.util.ParseNode;
import util.sys.FileProcessor;

public class SentenceListCreator extends FileProcessor<CCGParseList, SentenceList> {

	public SentenceListCreator() {
		super();
	}
	public SentenceListCreator(String inputDir, String outputDir) {
		super(inputDir, outputDir, new SentenceList());
	}
	@Override
	public int getNumFixedArgs() {
		return 0;
	}

	@Override
	public boolean hasNArgs() {
		return false;
	}

	@Override
	public String getConstructionErrorMsg() {
		return "SentenceListCreator requires no arguments.";
	}

	@Override
	public CCGParseList getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		try {
			return new CCGParseList(f.toPath(), true);
		} catch (ParseException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}

	@Override
	public void map(CCGParseList newData, SentenceList threadAggregate) {
		for (ParseNode pn : newData.getParser().getTops()) {
			threadAggregate.add(pn.getPhrase());
		}
		
	}

	@Override
	public void reduce(SentenceList threadAggregate) {
		synchronized(processAggregate) {
			processAggregate.addAll(threadAggregate);
		}
	}

}
