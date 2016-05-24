package edu.psu.acs.lang.production;

import java.util.ArrayList;

import edu.psu.acs.lang.declarative.ISlot;

public class ClearRetrievalError extends ProductionRule {

	public ClearRetrievalError() {
		super("FixRetrieval");
		this.requireSentence(new ArrayList<ISlot>());
		this.makeQuery(BufferQueries.checkRetrievalError());
		this.flushRetrieval();
	}

}
