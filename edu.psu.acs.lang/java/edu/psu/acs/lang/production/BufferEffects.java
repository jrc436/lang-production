package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.IModelElement;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;

public class BufferEffects implements IModelElement {
	private Buffer buffer;
	private List<ISlot> vars;
	private ChunkTypeEnum chunk; //should only be used in retrieval
	
	public BufferEffects(List<ISlot> vars, Buffer buff) {
		this.vars = vars;
		this.buffer = buff;
	}
	public BufferEffects(ChunkTypeEnum chunk, List<ISlot> vars, Buffer buff) {
		this.chunk = chunk;
		this.vars = vars;
		this.buffer = buff;
	}
	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		switch (buffer) {
			case goal:
				lines.add("<modify buffer=\""+buffer.toString()+"\">");
				break;
			case retrieval:
				lines.add("<add buffer=\""+buffer.toString()+"\" type=\""+chunk.toString()+"\">");
				break;
			default:
				throw new UnsupportedOperationException();
			}
		for (ISlot s : vars) {
			lines.add(s.toXML());
		}
		switch (buffer) {
			case goal:
				lines.add("</modify>");
				break;
			case retrieval:
				lines.add("</add>");
				break;
			default:
				throw new UnsupportedOperationException();
		}
		return lines;
	}

}
