package edu.psu.acs.lang.parsing;

import edu.psu.acs.lang.declarative.type.CCGType;

public class UnsupportedNode implements ParseNode {

	@Override
	public boolean validate(Object[] validators) {
		return false;
	}

	@Override
	public String typeToString() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getPhrase() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public CCGType getType() {
		// TODO Auto-generated method stub
		return null;
	}

}
