package edu.psu.acs.lang.declarative.type;

import java.util.ArrayList;

import edu.psu.acs.lang.declarative.chunk.ChunkStore;
import edu.psu.acs.lang.declarative.chunk.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.slot.Slot;

public class Conjable extends ChunkStore {
	private final ConjEnum conj;
	public Conjable(ConjEnum conj) {
		super(ChunkTypeEnum.conj, new ArrayList<Slot>());
		this.conj = conj;
	}
	public static Conjable factory() {
		return new Conjable(null);
	}
	public Conjable(boolean con) {
		this(ConjEnum.value(con));
	}
	@Override
	public boolean equals(Object other) {
		if (other == null || other.getClass() != this.getClass()) {
			return false;
		}
		return this.conj == ((Conjable)other).conj;
	}
	public String toString() {
		return conj.toString();
	}
	public ConjEnum valueOf(String s) {
		return ConjEnum.valueOf(s);
	}
	public boolean value(String s) {
		switch (valueOf(s)) {
			case Conjable:
				return true;
			case Nonconjable:
				return false;
			default:
				break;	
		}
		throw new IllegalArgumentException("String should represent the value of Conjable or Nonconjable");
	}

}
