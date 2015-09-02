package edu.psu.acs.lang.declarative;

public class SlotVar implements SlotValue {
	private String varName;
	public SlotVar(String varName) {
		if (varName.charAt(0) != '=') {
			System.err.println("A free variable should always begin with an equals sign.");
		}
		this.varName = varName;
	}
	public String toString() {
		return varName;
	}
}
