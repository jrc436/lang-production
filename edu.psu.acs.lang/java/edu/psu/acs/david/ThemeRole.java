package edu.psu.acs.david;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkStore;
import edu.psu.acs.lang.declarative.ChunkType;
import edu.psu.acs.lang.declarative.Slot;

public class ThemeRole extends ChunkStore {
	public static ThemeRole makeThemeRole(String name, ThemeRoleEnum type) {
		List<Slot> slots = new ArrayList<Slot>();
		slots.add(new Slot("slot", type));
		slots.add(new Slot("marker", type));
		slots.add(new Slot("marker-done", type));
		slots.add(new Slot("lexform", type));
		return new ThemeRole(name, ChunkType.themerole, slots);
	}	
	protected ThemeRole(String name, ChunkType chunkType, List<Slot> slots) {
		super(name, chunkType, slots);
	}
	
}
