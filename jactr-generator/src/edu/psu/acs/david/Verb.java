//package edu.psu.acs.david;
//
//import java.util.ArrayList;
//import java.util.List;
//
///**
// * Verbs are currently treated as the odd-ball, as there are specific methods to add them, but they are not chunks themselves
// * @author jrc
// *
// */
//public class Verb {
//	private Lexeme lexChunk;
//	private LexSynArgOrder ditransOrder1;
//	private LexSynArgOrder ditransOrder2;
//	public Verb(String name, String lex, VerbType type) {
//		lexChunk = Lexeme.makeVerbLexeme(name, lex);
//		ditransOrder1 = LexSynArgOrder.makeLexSynArgOrder(name, type, VerbOrder.first);
//		ditransOrder2 = LexSynArgOrder.makeLexSynArgOrder(name, type, VerbOrder.second);
//	}
//	public List<String> toXML() {
//		List<String> lines = new ArrayList<String>();
//		lines.addAll(ditransOrder1.toXML());
//		lines.addAll(ditransOrder2.toXML());
//		lines.addAll(lexChunk.toXML());
//		return lines;
//	}
//}
//
