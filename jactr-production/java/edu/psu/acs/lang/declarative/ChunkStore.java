package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.core.IModelElement;

public class ChunkStore implements IModelElement, SlotValue {
	/**
	 * This is the name of the chunk.
	 */
	//protected final String name;
	private final ChunkTypeEnum chunkType; //this is another odd instance of using an enum because java's type erasure isn't great
	private final List<Slot> slots;
	protected ChunkStore(ChunkTypeEnum chunkType, List<Slot> slots) {
		//this.name = name;
		this.slots = slots;
		this.chunkType = chunkType;
	}
	public List<String> toXML() {
		List<String> list = new ArrayList<String>();
		list.add("<chunk type=\""+chunkType.toString()+"\" name=\""+this.toString()+"\">");
		for (Slot s : slots) {
			list.add(s.toXML());
		}
		list.add("</chunk>");
		return list;
	}
	protected void addSlot(SlotName name, SlotValue sv) {
		slots.add(new Slot(name, sv));
	}
}
