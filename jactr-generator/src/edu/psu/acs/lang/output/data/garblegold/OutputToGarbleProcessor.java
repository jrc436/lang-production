package edu.psu.acs.lang.output.data.garblegold;

import java.io.File;
import java.io.IOException;

import edu.psu.acs.lang.output.data.tree.GarbleTreeList;
import util.sys.FileProcessor;

public class OutputToGarbleProcessor extends FileProcessor<GarbleTreeList, GarbleTreeList> {
	public OutputToGarbleProcessor() {
		super();
	}
	public OutputToGarbleProcessor(String inpDir, String outDir) {
		super(inpDir, outDir, new GarbleTreeList());
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
		return "requires no arguments";
	}

	@Override
	public GarbleTreeList getNextData() {
		File f = super.getNextFile();
		if (f == null) {
			return null;
		}
		try {
			return GarbleTreeList.readOutputFile(f);
		}
		catch (IOException io) {
			io.printStackTrace();
			System.exit(1);
		}
		return null;
	}

	@Override
	public void map(GarbleTreeList newData, GarbleTreeList threadAggregate) {
		threadAggregate.absorb(newData);
	}

	@Override
	public void reduce(GarbleTreeList threadAggregate) {
		synchronized (processAggregate) {
			processAggregate.absorb(threadAggregate);
		}
		
	}

}
