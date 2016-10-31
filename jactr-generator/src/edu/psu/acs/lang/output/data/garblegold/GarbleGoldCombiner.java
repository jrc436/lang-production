package edu.psu.acs.lang.output.data.garblegold;

import java.io.File;
import java.io.IOException;

import util.sys.FileProcessor;

public class GarbleGoldCombiner extends FileProcessor<GarbleGoldList, GarbleGoldList> {
	public GarbleGoldCombiner() {
		super();
	}
	public GarbleGoldCombiner(String inpDir, String outDir) {
		super(inpDir, outDir, new GarbleGoldList());
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
		return "GarbleGoldCombiner needs a path to an inputdirectory containing gold and garble files.";
	}

	@Override
	public GarbleGoldList getNextData() {
		File f = super.getNextFile();
		if (f == null) {
			return null;
		}
		try {
			return GarbleGoldList.readAnyFile(f);
		}
		catch (IOException io) {
			io.printStackTrace();
			System.exit(1);
		}
		return null;
	}

	@Override
	public void map(GarbleGoldList newData, GarbleGoldList threadAggregate) {
		threadAggregate.absorb(newData);
	}

	@Override
	public void reduce(GarbleGoldList threadAggregate) {
		synchronized(processAggregate) {
			processAggregate.absorb(threadAggregate);
		}
	}

}
