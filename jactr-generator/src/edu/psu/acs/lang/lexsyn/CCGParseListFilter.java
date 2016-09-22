package edu.psu.acs.lang.lexsyn;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import edu.psu.acs.lang.util.ParseException;
import util.sys.FileProcessor;

public class CCGParseListFilter extends FileProcessor<CCGParseList, CCGParseList> {

	@Override
	public int getNumFixedArgs() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean hasNArgs() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public String getConstructionErrorMsg() {
		// TODO Auto-generated method stub
		return null;
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

	@Override
	public void map(CCGParseList newData, CCGParseList threadAggregate) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void reduce(CCGParseList threadAggregate) {
		// TODO Auto-generated method stub
		
	}

}
