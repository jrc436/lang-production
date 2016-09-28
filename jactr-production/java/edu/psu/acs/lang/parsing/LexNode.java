package edu.psu.acs.lang.parsing;

import edu.psu.acs.lang.declarative.CCGType;

public class LexNode implements ParseNode {
	private String word;
	private CCGType type;
	public LexNode(String word, CCGType type) {
		this.word = word.toLowerCase();
		this.type = type;
	}
	public boolean equals(Object o) {
		if (o == null || o.getClass().equals(this.getClass())) {
			return false;
		}
		LexNode l = (LexNode) o;
		return l.word.equals(this.word) && l.type.equals(this.type);
	}
	public int hashCode() {
		return 49*word.hashCode() + type.hashCode();
	}
	@Override
	public String typeToString() {
		return type.toString();
	}
	@Override
	public CCGType getType() {
		return type;
	}
	@Override
	public String getPhrase() {
		return word;
	}
	@Override
	public String toString() {
		return "{"+type.toString()+" "+word+"}";
	}
	@Override
	public boolean validate(Object[] validators) {
		if (validators.length != 2) {
			throw new IllegalArgumentException("LexNode has 2 validators, 1: word, 2: type");
		}
		if (!(validators[0] instanceof String) || !(validators[1] instanceof String)) {
			throw new IllegalArgumentException("Both of LexNode's validators are Strings");
		}
		return word.equals(validators[0]) && type.toString().equals(validators[1]);
	}
}
