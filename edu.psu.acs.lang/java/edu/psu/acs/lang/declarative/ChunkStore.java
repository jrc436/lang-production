package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;

public class ChunkStore {
	/**
	 * This is the name of the chunk.
	 */
	protected final String name;
	private final ChunkType chunkType; //this is another odd instance of using an enum because java's type erasure isn't great
	private final List<Slot> slots;
	protected ChunkStore(String name, ChunkType chunkType, List<Slot> slots) {
		this.name = name;
		this.slots = slots;
		this.chunkType = chunkType;
	}
	public List<String> toXML() {
		List<String> list = new ArrayList<String>();
		list.add("chunk type=\""+chunkType.toString()+"\" name=\""+name+"\">");
		for (Slot s : slots) {
			list.add(s.toXML());
		}
		list.add("</chunk>");
		return list;
	}
}
