package edu.psu.acs.lang.parsing;

import edu.psu.acs.lang.declarative.CCGType;
import edu.psu.acs.lang.tracing.Equivocable;

public interface ParseNode extends Equivocable {
	public String typeToString();
	public String getPhrase();
	public CCGType getType();
}
