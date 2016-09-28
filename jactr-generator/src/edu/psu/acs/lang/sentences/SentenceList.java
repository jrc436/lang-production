package edu.psu.acs.lang.sentences;

import java.util.ArrayList;
import java.util.Iterator;

import util.sys.DataType;

public class SentenceList extends ArrayList<String> implements DataType {
	/**
	 * 
	 */
	private static final long serialVersionUID = 7610951695401883228L;

	public SentenceList() {
		super();
	}
	public SentenceList(SentenceList s) {
		super(s);
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
		return "SentenceDataType requires no args";
	}

	@Override
	public String getFileExt() {
		return ".list";
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		return this;
	}

	@Override
	public String getHeaderLine() {
		return null;
	}

	@Override
	public String getFooterLine() {
		return null;
	}

	@Override
	public Iterator<String> getStringIter() {
		return this.iterator();
	}

	@Override
	public DataType deepCopy() {
		return new SentenceList(this);
	}

}
