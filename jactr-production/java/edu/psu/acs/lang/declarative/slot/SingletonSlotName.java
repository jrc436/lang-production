package edu.psu.acs.lang.declarative.slot;

public class SingletonSlotName implements SlotName {
	private final String singleName;
	public SingletonSlotName(SingletonSlotNameEnum name) {
		this.singleName = name.toString();
	}
	public String toString() {
		return singleName;
	}
}
