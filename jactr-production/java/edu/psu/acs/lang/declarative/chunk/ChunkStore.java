package edu.psu.acs.lang.declarative.chunk;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.slot.NullValue;
import edu.psu.acs.lang.declarative.slot.Slot;
import edu.psu.acs.lang.declarative.slot.SlotName;
import edu.psu.acs.lang.declarative.slot.SlotValue;

public class ChunkStore implements IChunkStore {
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
	@Override
	public List<String> chunkTypeXML() {
		List<String> toRet = new ArrayList<String>();
		toRet.add("<chunk-type name=\""+chunkType.toString()+"\">");
		for (Slot s : slots) {
			toRet.add("<slot name=\""+s.getName().toString()+"\" equals=\""+new NullValue().toString()+"\"/>");
		}
		toRet.add("</chunk-type>");
		return toRet;
	}
}
