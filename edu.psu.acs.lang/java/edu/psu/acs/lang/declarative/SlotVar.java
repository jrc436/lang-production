package edu.psu.acs.lang.declarative;

public class SlotVar implements SlotValue {
	private String varName;
	/**
	 * A free variable in a slot
	 * @param varName
	 */
	public SlotVar(String varName) {
		this.varName = varName;
	}
	public String toString() {
		return "="+varName;
	}
}
