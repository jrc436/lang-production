package edu.psu.acs.david;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkStore;
import edu.psu.acs.lang.declarative.ChunkType;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.StringValue;

public class Syntype extends ChunkStore {

	public Syntype makeComplexWithAttract(CCGSyntypeEnum name, CCGSyntypeEnum left, CCGSyntypeEnum right, CCGCombinatorEnum combo, CCGSyntypeEnum attract, String ccgbankType) {
		List<Slot> slots = baseSlotList(left, right, combo, ccgbankType);
		slots.add(new Slot("attract", attract));
		return new Syntype(name.toString(), ChunkType.syntype, slots);
	}
	public Syntype makeComplexSyntype(CCGSyntypeEnum name, CCGSyntypeEnum left, CCGSyntypeEnum right, CCGCombinatorEnum combo, String ccgbankType) {
		List<Slot> slots = baseSlotList(left, right, combo, ccgbankType);
		return new Syntype(name.toString(), ChunkType.syntype, slots);
	}
	public Syntype makeBasicSyntype(CCGSyntypeEnum name) {
		List<Slot> slots = new ArrayList<Slot>();
		slots.add(new Slot("class", SyntypeClass.Basic));
		return new Syntype(name.toString(), ChunkType.syntype, slots);
		
	}
	private List<Slot> baseSlotList(CCGSyntypeEnum left, CCGSyntypeEnum right, CCGCombinatorEnum combo, String ccgbankType) {
		List<Slot> slots = new ArrayList<Slot>();
		slots.add(new Slot("class", SyntypeClass.Complex));
		slots.add(new Slot("left", left));
		slots.add(new Slot("right", right));
		slots.add(new Slot("comb", combo));
		slots.add(new Slot("ccgbankType", new StringValue(ccgbankType)));
		return slots;
	}
	
	protected Syntype(String name, ChunkType chunkType, List<Slot> slots) {
		super(name, chunkType, slots);
	}

}
