package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.core.IModelElement;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;

/**
 * What is required for a production to be fired. Notice that sometimes, the slot value or slot name is stored as a variable with "="
 * These variables should be passed with that value
 * @author jrc
 *
 */
public class BufferConditions implements IModelElement {
	protected final Buffer buffer;
	private List<ISlot> toMatch; 
	private ChunkTypeEnum chunk;
	public BufferConditions(Buffer buffer, ChunkTypeEnum type, List<ISlot> vars) {
		this.buffer = buffer;
		chunk = type;
		this.toMatch = vars;
	}

	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		lines.add("<match buffer=\""+buffer.toString()+"\" type=\""+chunk.toString()+"\">");
		for (ISlot s : toMatch) {
			lines.add(s.toXML());
		}
		lines.add("</match>");
		return lines;
	}

}
