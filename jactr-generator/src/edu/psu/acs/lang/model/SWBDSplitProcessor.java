package edu.psu.acs.lang.model;

import java.io.File;

import edu.psu.acs.lang.ccg.CCGParseList;
import edu.psu.acs.lang.parsing.ParseException;
import util.data.BigDataSplitter;

public class SWBDSplitProcessor extends BigDataSplitter<CCGParseList> {
	public SWBDSplitProcessor() {
		super();
	}
	public SWBDSplitProcessor(String inpDir, String outDir) {
		super(inpDir, outDir, new CCGParseList());
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
			//System.exit(1);
		}
		return null;
	}

}
