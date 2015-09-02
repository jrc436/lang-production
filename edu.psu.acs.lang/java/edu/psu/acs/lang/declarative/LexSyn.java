package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;

public class LexSyn extends ChunkStore implements SlotValue {
	public LexSyn makeLexSyn(String name, List<CCGType> potentialSlots) {
		List<Slot> slots = new ArrayList<Slot>();
		slots.add(new Slot("word", new StringValue(name)));
		int i = 0;
		for (CCGType t : potentialSlots) {
			slots.add(new Slot("type"+i, t));
			i++;
		}
		return new LexSyn(name, ChunkType.lexsyn, slots);
	}
	protected LexSyn(String name, ChunkType chunkType, List<Slot> slots) {
		super(name, chunkType, slots);
	}

}
