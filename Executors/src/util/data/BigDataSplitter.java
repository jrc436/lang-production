package util.data;

import java.io.File;

import util.sys.DataType;
import util.sys.FileProcessor;

public abstract class BigDataSplitter<E extends DataType> extends FileProcessor<E, E> {
	private Integer filecounter;
	public BigDataSplitter() {
		filecounter = 0;
	}
	public BigDataSplitter(String inpDir, String outDir, E outtype) {
		super(inpDir, outDir, outtype);
		filecounter = 0;
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
		return "BigDataSplitters require no further arguments";
	}

	@Override
	public void map(E newData, E threadAggregate) {
		int filenum = 0;
		synchronized(filecounter) {
			filenum = filecounter;
			filecounter = this.writeData(newData, filenum);;
		}
	}

	@Override
	public void reduce(E threadAggregate) {
		//Do nothing. We're leaving a big ol' empty processAggregate.
	}
	@Override
	public File write() {
		//do nothing
		return outputDir.toFile();
	}
	
}
