package edu.psu.acs.lang.declarative.chunk;

import java.util.ArrayList;

import edu.psu.acs.lang.declarative.slot.Slot;

/**
 * this is a hack to get around not matching null chunks. Chunks that should be matched, but are still empty are called "empty"
 * @author jrc
 *
 */
public class EmptyChunk extends ChunkStore {
	private final EmptyEnum en;
	public EmptyChunk(EmptyEnum en) {
		super(ChunkTypeEnum.empty, new ArrayList<Slot>());
		// TODO Auto-generated constructor stub
		this.en = en;
	}
	public String toString() {
		return en.toString();
	}
	public static EmptyChunk factory() {
		return new EmptyChunk(null);
	}

}
