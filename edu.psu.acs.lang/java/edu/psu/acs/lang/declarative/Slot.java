package edu.psu.acs.lang.declarative;

/**
 * A storage mechanism for one slot of a chunk.
 * @author jrc
 *
 */
public class Slot implements ISlot {
	private final SlotName name;
	private final SlotValue value;
	public Slot(SlotName name, SlotValue value) {
		this.name = name;
		this.value = value;
	}
	public String toXML() {
		return "<slot name=\""+name+"\" equals=\""+value.toString()+"\"/>";
	}
}
