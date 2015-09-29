package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.IModelElement;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.Slot;

public class BufferEffects implements IModelElement {
	private Buffer buffer;
	private List<Slot> vars;
	private ChunkTypeEnum chunk; //should only be used in retrieval
	public static BufferEffects makeRetrieval(ChunkTypeEnum chunk, List<Slot> vars) {
		return new BufferEffects(chunk, vars, Buffer.retrieval);
	}
	public static BufferEffects modifyGoal(List<Slot> vars) {
		return new BufferEffects(vars, Buffer.goal);
	}
	private BufferEffects(List<Slot> vars, Buffer buff) {
		this.vars = vars;
		this.buffer = buff;
	}
	private BufferEffects(ChunkTypeEnum chunk, List<Slot> vars, Buffer buff) {
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
		for (Slot s : vars) {
			lines.add("<slot name=\""+s.getName()+"\" equals=\""+s.getValue()+"/>");
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
