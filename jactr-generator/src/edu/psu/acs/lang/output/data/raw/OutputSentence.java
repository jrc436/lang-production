package edu.psu.acs.lang.output.data.raw;

import java.util.ArrayList;


public class OutputSentence extends ArrayList<IDWordPair> {
	private static final long serialVersionUID = -3269605362834674916L;
	public boolean add(String garble) {
		return super.add(new IDWordPair(garble));
	}
	public boolean contains(String garble) {
		return super.contains(new IDWordPair(garble));
	}
}
