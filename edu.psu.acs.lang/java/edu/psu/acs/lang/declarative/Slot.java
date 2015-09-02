package edu.psu.acs.lang.declarative;

/**
 * A storage mechanism for one slot of a chunk.
 * @author jrc
 *
 */
public class Slot {
	private final String name;
	private final SlotValue value;
	public Slot(String name, SlotValue value) {
		this.name = name;
		this.value = value;
	}
	public String toXML() {
		return "<slot name=\""+name+"\" equals=\""+value.toString()+"\"/>";
	}
	
	//mostly for use in production
	public String getName() {
		return name;
	}
	public String getValue() {
		return value.toString();
	}
}
