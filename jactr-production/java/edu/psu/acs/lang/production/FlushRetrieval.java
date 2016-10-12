package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.slot.ISlot;

public class FlushRetrieval extends BufferEffects {
	public FlushRetrieval() {
		super(new ArrayList<ISlot>(), Buffer.retrieval);
	}
	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		lines.add("<remove buffer=\""+buffer.toString()+"\"/>");
		return lines;
	}
}
