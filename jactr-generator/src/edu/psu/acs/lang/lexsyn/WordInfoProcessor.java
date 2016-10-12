package edu.psu.acs.lang.lexsyn;

import java.io.File;

import edu.psu.acs.lang.ccg.CCGParseList;
import edu.psu.acs.lang.parsing.ParseException;
import util.sys.FileProcessor;

public class WordInfoProcessor extends FileProcessor<CCGParseList, LexsynOrderedList> {
	public WordInfoProcessor(String inpDir, String outDir) {
		super(inpDir, outDir, new LexsynOrderedList());
	}
	public WordInfoProcessor() {
		super();
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
		return "WordInfoProcessor requires no futher arguments";
	}

	@Override
	public CCGParseList getNextData() {
		File f = super.getNextFile();	
		//System.out.println(f);
		if ( f == null) {
			return null;
		}		
		try {
			return new CCGParseList(f.toPath(), false);
		} catch (ParseException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}
	

	@Override
	public void map(CCGParseList newData, LexsynOrderedList threadAggregate) {
		threadAggregate.absorb(new LexsynOrderedList(newData.getParser().wordTypes()));
	}

	@Override
	public void reduce(LexsynOrderedList threadAggregate) {
		synchronized(processAggregate) {
			processAggregate.absorb(threadAggregate);
		}
	}

}
