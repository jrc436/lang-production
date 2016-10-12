package edu.psu.acs.lang.ccg;

import edu.psu.acs.lang.parsing.ParseNode;

@FunctionalInterface
public interface Grokker {
	public ParseNode grok(ParseNode pn);
}
