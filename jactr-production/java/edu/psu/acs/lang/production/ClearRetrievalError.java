package edu.psu.acs.lang.production;

public class ClearRetrievalError extends ProductionRule {

	public ClearRetrievalError() {
		super("FixRetrieval");
		this.makeQuery(BufferQueries.checkRetrievalError());
		this.flushRetrieval();
	}

}
