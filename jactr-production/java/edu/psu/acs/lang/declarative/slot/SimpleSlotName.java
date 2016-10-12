package edu.psu.acs.lang.declarative.slot;

public class SimpleSlotName implements SlotName {
	private final SimpleSlotNameEnum base;
	private final int addendum;
	public SimpleSlotName(SimpleSlotNameEnum base, int number) {
		this.base = base;
		addendum = number;
	}
	@Override
	public String toString() {
		return base.toString()+addendum;
	}
}
