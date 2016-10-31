package edu.psu.acs.lang.output.data.text;

import java.io.File;
import java.io.IOException;
import java.util.Map.Entry;

import edu.psu.acs.lang.output.data.tree.GarbleTree;
import edu.psu.acs.lang.output.data.tree.GarbleTreeList;
import edu.psu.acs.lang.sentences.SentenceList;
import util.sys.FileProcessor;

public class OutputToSentencesProcessor extends FileProcessor<GarbleTreeList, SentenceList> {

	public OutputToSentencesProcessor(String inpDir, String outDir) {
		super(inpDir, outDir, new SentenceList());
	}
	public OutputToSentencesProcessor() {
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
		return "OutputToSentencesProcessor requires no arguments.";
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
	public void map(GarbleTreeList newData, SentenceList threadAggregate) {
		for (Entry<Integer, GarbleTree> gt : newData.entrySet()) {
			threadAggregate.put(gt.getKey(), gt.getValue().getDelimitedFragLine());
		}
		System.err.println("mapped");
	}

	@Override
	public void reduce(SentenceList threadAggregate) {
		synchronized(processAggregate) {
			processAggregate.addAll(threadAggregate);
		}
		System.err.println("reduced");
	}

}
