package edu.psu.acs.lang.gen;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import edu.psu.acs.lang.lexsyn.CCGParseList;
import edu.psu.acs.lang.util.ParseException;
import util.data.BigDataSplitter;

public class SWBDSplitProcessor extends BigDataSplitter<CCGParseList> {
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
			return new CCGParseList(Files.readAllLines(f.toPath()), true);
		} catch (ParseException | IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}

}
