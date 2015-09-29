//package edu.psu.acs.david;
//
//import java.util.ArrayList;
//import java.util.List;
//
//import edu.psu.acs.lang.declarative.ChunkStore;
//import edu.psu.acs.lang.declarative.ChunkType;
//import edu.psu.acs.lang.declarative.Slot;
//
//public class Combination extends ChunkStore {
//	protected Combination(String name2, List<Slot> slots, ChunkType combination) {
//		super(name2, combination, slots);
//	}
//	public static Combination make_combination(String name, CCGSyntypeEnum left, CCGSyntypeEnum leftComb, CCGSyntypeEnum leftLeft, CCGSyntypeEnum leftRight, CCGSyntypeEnum right, CCGSyntypeEnum rightComb, CCGSyntypeEnum rightLeft, CCGSyntypeEnum rightRight) {
//		List<Slot> slots = new ArrayList<Slot>();
//		slots.add(new Slot("left", left));
//		slots.add(new Slot("leftComb", leftComb));
//		slots.add(new Slot("leftLeft", leftLeft));
//		slots.add(new Slot("leftRight", leftRight));
//		slots.add(new Slot("right", right));
//		slots.add(new Slot("rightComb", rightComb));
//		slots.add(new Slot("rightLeft", rightLeft));
//		slots.add(new Slot("rightRight", rightRight));
//		return new Combination(name, slots, ChunkType.combination);
//	}
//}
