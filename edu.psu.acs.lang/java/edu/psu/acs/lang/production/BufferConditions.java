package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkType;
import edu.psu.acs.lang.IModelElement;
import edu.psu.acs.lang.declarative.Slot;

/**
 * What is required for a production to be fired. Notice that sometimes, the slot value or slot name is stored as a variable with "="
 * These variables should be passed with that value
 * @author jrc
 *
 */
public class BufferConditions implements IModelElement {
	private final Buffer buffer;
	private List<Slot> toMatch;
	private List<Slot> freeVars; 
	private ChunkType chunk;
	public BufferConditions(Buffer buffer, ChunkType type, List<Slot> toMatch, List<Slot> freeVars) {
		this.buffer = buffer;
		chunk = type;
		this.toMatch = toMatch;
		this.freeVars = freeVars;
	}

	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		lines.add("<match buffer=\""+buffer.toString()+"\" type=\""+chunk.toString()+"\">");
		for (Slot s : toMatch) {
			lines.add("<slot name=\""+s.getName()+"\" equals=\""+s.getValue()+"/>");
		}
		for (Slot s : freeVars) {
			lines.add("<slot name=\""+s.getName()+"\" equals=\""+s.getValue()+"/>");
		}
		lines.add("</match>");
		return lines;
	}

}
