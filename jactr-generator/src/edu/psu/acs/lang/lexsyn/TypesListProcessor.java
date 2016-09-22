package edu.psu.acs.lang.lexsyn;

import util.sys.FileProcessor;

public class TypesListProcessor extends FileProcessor<CCGParseList, TypesList> {

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
	public CCGParseList getNextData() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void map(CCGParseList newData, TypesList threadAggregate) {
		
		
	}

	@Override
	public void reduce(TypesList threadAggregate) {
		// TODO Auto-generated method stub
		
	}

}
