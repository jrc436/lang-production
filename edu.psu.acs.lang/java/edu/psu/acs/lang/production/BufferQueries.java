package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

/**
 * This class is a bit of a hack. Oh well!
 * @author jrc
 *
 */
public class BufferQueries extends BufferConditions {
	private String type;
	private BufferQueryEnum req;
	public BufferQueries(Buffer buffer, String type, BufferQueryEnum req) {
		super(buffer, null, null);
		this.type = type;
		this.req = req;
	}
	public static BufferQueries checkRetrievalEmpty() {
		return new BufferQueries(Buffer.retrieval, BufferValueEnum.empty.type(), BufferValueEnum.empty);
	}
	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		lines.add("<query buffer=\""+buffer.toString()+"\">");
		lines.add("<slot name=\""+type+"\" equals=\""+req.toString()+"\"/>");	
		lines.add("</query>");
		return lines;
	}

}
