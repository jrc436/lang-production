package edu.psu.acs.lang.declarative;


public class StringValue implements SlotValue {
	private final String string;
	public StringValue(String str) {
		this.string = str;
	}
	public String toString() {
		return string;
	}
}
