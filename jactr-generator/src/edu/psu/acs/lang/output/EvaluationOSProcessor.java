package edu.psu.acs.lang.output;

import java.io.File;
import java.io.IOException;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGold;
import edu.psu.acs.lang.eval.data.garblegold.GarbleGoldList;
import util.sys.FileProcessor;

public class EvaluationOSProcessor extends FileProcessor<GarbleGoldList, EvaluationOSSet> {
	public EvaluationOSProcessor() {
		super();
	}
	public EvaluationOSProcessor(String inpDir, String outDir, String[] clses) {
		super(inpDir, outDir, EvaluationOSSet.makeSet(clses));
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
		return "The Evaluation Processor requires no args beyond the EvaluationSet's evaluators";
	}

	@Override
	public GarbleGoldList getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		try {
			return GarbleGoldList.readGGFile(f);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}

	@Override
	public void map(GarbleGoldList newData, EvaluationOSSet threadAggregate) {
		for (GarbleGold s : newData.values()) {
			threadAggregate.add(s);
		}
	}

	@Override
	public void reduce(EvaluationOSSet threadAggregate) {
		synchronized(processAggregate) {
			for (GarbleGold key : threadAggregate.keySet()) {
				processAggregate.put(key, threadAggregate.get(key));
			}
		}
	}

}
