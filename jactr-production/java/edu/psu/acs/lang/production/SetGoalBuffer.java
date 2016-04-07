package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ISlot;

public class SetGoalBuffer extends ChangeGoalBuffer {
	public SetGoalBuffer(String newChunk) {
		super(newChunk, new ArrayList<ISlot>());
	}
	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		lines.add("<add buffer=\""+buffer.toString()+"\" chunk=\""+nameOfNewGoalChunk+"\"/>");
		return lines;
	}
}
