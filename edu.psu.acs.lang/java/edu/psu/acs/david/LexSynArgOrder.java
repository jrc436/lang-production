//package edu.psu.acs.david;
//
//import java.util.ArrayList;
//import java.util.List;
//
//import org.apache.commons.lang.NotImplementedException;
//
//import edu.psu.acs.lang.declarative.ChunkStore;
//import edu.psu.acs.lang.declarative.ChunkType;
//import edu.psu.acs.lang.declarative.Slot;
//import edu.psu.acs.lang.declarative.StringValue;
//
//////kind of a stretch goal. This is basically sentence orders. It is not entirely clear if these should even actually be chunks, as they are
//////quite possibly procedural memory.
//
//public class LexSynArgOrder extends ChunkStore {
//	public static LexSynArgOrder makeLexSynArgOrder(String name, VerbType verbtype, VerbOrder order) {
//		String suffix = "-ORDER";
//		String prefix = "";
//		switch (verbtype) {
//			case Ditransitive:
//				prefix = "-DITRANS";
//				break;
//			case Transitive:
//				prefix = "-TRANS";
//				break;
//			default:
//				break;
//		}
//		CCGSyntypeEnum forsyn;
//		switch (order) {
//			case first:
//				forsyn = CCGSyntypeEnum.DiTrans;
//				break;
//			case second:
//				prefix += "TO";
//				forsyn = CCGSyntypeEnum.DiTransTo;
//				break;
//			default:
//				throw new NotImplementedException("This verb order is not yet supported, please add the case!");		
//		}
//		List<Slot> slots = new ArrayList<Slot>();
//		slots.add(new Slot("for-syn", forsyn));
//		slots.add(new Slot("for-lexeme", new StringValue(name+"SEM")));
//		return new LexSynArgOrder(name+prefix+suffix, ChunkType.lexSynArgOrder, slots);
//	}
//	protected LexSynArgOrder(String name, ChunkType chunkType, List<Slot> slots) {
//		super(name, chunkType, slots);
//		// TODO Auto-generated constructor stub
//	}
//
//}
