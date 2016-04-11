package edu.psu.acs.lang.util;

import edu.psu.acs.lang.Equivocable;
import edu.psu.acs.lang.declarative.CCGType;

public interface ParseNode extends Equivocable {
	public String typeToString();
	public String getPhrase();
	public CCGType getType();
}
