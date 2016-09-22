package edu.psu.acs.lang.lexsyn;

import java.util.ArrayList;
import java.util.Iterator;

import edu.psu.acs.lang.declarative.CCGType;
import util.sys.DataType;

public class TypesList extends ArrayList<CCGType> implements DataType {

	private static final long serialVersionUID = 134825138165028464L;

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
	public String getFileExt() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getHeaderLine() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getFooterLine() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Iterator<String> getStringIter() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public DataType deepCopy() {
		// TODO Auto-generated method stub
		return null;
	}

}
