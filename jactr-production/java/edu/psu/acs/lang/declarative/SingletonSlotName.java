package edu.psu.acs.lang.declarative;

public class SingletonSlotName implements SlotName {
	private final String singleName;
	public SingletonSlotName(SingletonSlotNameEnum name) {
		this.singleName = name.toString();
	}
	public String toString() {
		return singleName;
	}
}
