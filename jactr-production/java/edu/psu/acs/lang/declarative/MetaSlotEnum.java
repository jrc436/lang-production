package edu.psu.acs.lang.declarative;

public enum MetaSlotEnum implements SlotName {
	recentlyRetrieved,
	retrievalThreshhold,
	partialMatch,
	indexedRetrieval;
	public String toString() {
		String s = "";
		switch (this) {
			case indexedRetrieval:
				s = "indexedRetrieval";
				break;
			case partialMatch:
				s = "partialMatch";
				break;
			case recentlyRetrieved:
				s = "recently-retrieved";
				break;
			case retrievalThreshhold:
				s = "retrievalThreshold";
				break;
			default:
				break;
		}
		return ":"+s;
	}
}
