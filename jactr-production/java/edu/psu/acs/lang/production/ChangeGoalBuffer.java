package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ISlot;

public class ChangeGoalBuffer extends BufferEffects {
	protected String nameOfNewGoalChunk;
	public ChangeGoalBuffer(String nameOfNewGoalChunk, List<ISlot> removeEffects) {
		super(removeEffects, Buffer.goal);
		this.nameOfNewGoalChunk = nameOfNewGoalChunk;
	}
	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		lines.add("<remove buffer=\""+buffer.toString()+"\">");
		for (ISlot s : vars) {
			lines.add(s.toXML());
		}
		lines.add("</remove>");
		lines.add("<add buffer=\""+buffer.toString()+"\" chunk=\""+nameOfNewGoalChunk+"\"/>");
		return lines;
	}
}
