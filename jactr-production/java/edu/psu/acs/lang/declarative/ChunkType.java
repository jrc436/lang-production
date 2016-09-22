package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.core.IModelElement;

public class ChunkType implements IModelElement {
	private ChunkTypeEnum name;
	private List<SlotName> slotNames;
	public ChunkType(ChunkTypeEnum name, List<SlotName> slotNames) {
		this.name = name;
		this.slotNames = slotNames;
	}
	
	@Override
	public List<String> toXML() {
		List<String> toRet = new ArrayList<String>();
		toRet.add("<chunk-type name=\""+name.toString()+"\">");
		for (SlotName s : slotNames) {
			toRet.add("<slot name=\""+s.toString()+"\" equals=\""+new NullValue().toString()+"\"/>");
		}
		toRet.add("</chunk-type>");
		return toRet;
	}

}
