//package edu.psu.acs.david;
//
//import java.util.ArrayList;
//import java.util.List;
//
//import edu.psu.acs.lang.declarative.ChunkStore;
//import edu.psu.acs.lang.declarative.ChunkType;
//import edu.psu.acs.lang.declarative.Slot;
//import edu.psu.acs.lang.declarative.StringValue;
//
//public class Lexeme extends ChunkStore {
//	public static Lexeme makeVerbLexeme(String name, String lex) {
//		List<Slot> slots = baseSlots(name, name, lex, CCGSyntypeEnum.Nullsyn);
//		return new Lexeme(name+"SEM", ChunkType.lexeme, slots);
//	}
//	public static Lexeme makeLexeme(String name, String sem, String lex, String ccgbankType) {
//		return makeLexeme(name, sem, lex, ccgbankType, CCGSyntypeEnum.Nullsyn);
//	}
//	public static Lexeme makeLexeme(String name, String sem, String lex, String ccgbankType, CCGSyntypeEnum syn) {
//		List<Slot> slots = baseSlots(name, sem, lex, syn);
//		slots.add(new Slot("ccgbankType", new StringValue(ccgbankType)));
//		return new Lexeme(name, ChunkType.lexeme, slots);
//	}
//	private static List<Slot> baseSlots(String name, String sem, String lex, CCGSyntypeEnum syn) {
//		List<Slot> slots = new ArrayList<Slot>();
//		slots.add(new Slot("sem", new StringValue(name)));
//		slots.add(new Slot("syn", syn));
//		slots.add(new Slot("lex", new StringValue(lex)));
//		return slots;
//	}
//	protected Lexeme(String name, ChunkType chunkType, List<Slot> slots) {
//		super(name, chunkType, slots);
//	}
//
//}
