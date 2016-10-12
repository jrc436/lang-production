package edu.psu.acs.lang.types;


import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

import edu.psu.acs.lang.declarative.type.CCGType;
import util.sys.DataType;
import util.sys.FileWritable;

public class TypesList extends HashSet<CCGType> implements DataType {
	public TypesList() {
		super();
	}
	public TypesList(TypesList tl) {
		super(tl);
	}
	private static final long serialVersionUID = 134825138165028464L;

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
		return "TypesList requires no args";
	}

	@Override
	public String getFileExt() {
		return ".list";
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> l = new ArrayList<String>();
		for (CCGType cg : this) {
			l.add(cg.toString());
		}
		return l;
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
		return FileWritable.iterBuilder(this);
	}

	@Override
	public DataType deepCopy() {
		return new TypesList(this);
	}

}
