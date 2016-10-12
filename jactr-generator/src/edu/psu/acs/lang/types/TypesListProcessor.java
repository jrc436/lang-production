package edu.psu.acs.lang.types;

import java.io.File;
import java.io.IOException;

import edu.psu.acs.lang.declarative.type.CCGType;
import edu.psu.acs.lang.lexsyn.LexsynOrderedList;
import edu.psu.acs.lang.parsing.ParseException;
import util.sys.FileProcessor;

public class TypesListProcessor extends FileProcessor<LexsynOrderedList, TypesList> {
	public TypesListProcessor(String inpDir, String outDir) {
		super(inpDir, outDir, new TypesList());
	}
	public TypesListProcessor() {
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
		return "TypesListProcessor needs no further arguments";
	}
	

	@Override
	public LexsynOrderedList getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		LexsynOrderedList dat = null;
		try {
			dat = LexsynOrderedList.createFromFile(f.toPath());
		} catch (IOException | ParseException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return dat;
	}

	@Override
	public void map(LexsynOrderedList newData, TypesList threadAggregate) {
		for (CCGType cg : newData.getKeysetTwo()) {
			threadAggregate.add(cg);
		}
		
	}

	@Override
	public void reduce(TypesList threadAggregate) {
		synchronized (processAggregate) {
			processAggregate.addAll(threadAggregate);
		}
		
	}

}
