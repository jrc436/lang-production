package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang.NotImplementedException;

import edu.psu.acs.lang.declarative.ChunkType;
import edu.psu.acs.lang.IModelElement;
import edu.psu.acs.lang.declarative.Slot;

public class BufferEffects implements IModelElement {
	private Buffer buffer;
	private List<Slot> fixValued;
	private List<Slot> varValued;
	private ChunkType chunk; //should only be used in retrieval
	public static BufferEffects makeRetrieval(ChunkType chunk, List<Slot> toMatchFixValued, List<Slot> toMatchVarValued) {
		return new BufferEffects(chunk, toMatchFixValued, toMatchVarValued);
	}
	public static BufferEffects modifyGoal(List<Slot> fixValuedMods, List<Slot> varValuedMods) {
		return new BufferEffects(fixValuedMods, varValuedMods);
	}
	private BufferEffects(List<Slot> fixValues, List<Slot> varValues) {
		this.fixValued = fixValues;
		this.varValued = varValues;
	}
	private BufferEffects(ChunkType chunk, List<Slot> toMatchFixValued, List<Slot> toMatchVarValued) {
		this.chunk = chunk;
		this.fixValued = toMatchFixValued;
		this.varValued = toMatchVarValued;
		this.buffer = Buffer.retrieval;
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
			throw new NotImplementedException();
		}
		for (Slot s : fixValued) {
			lines.add("<slot name=\""+s.getName()+"\" equals=\""+s.getValue()+"/>");
		}
		for (Slot s : varValued) {
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
			throw new NotImplementedException();
		}
		return lines;
	}

}
