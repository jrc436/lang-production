package edu.psu.acs.lang.declarative;

import java.util.ArrayList;

public class Conjable extends ChunkStore implements SlotValue {
	private final ConjEnum conj;
	public Conjable(ConjEnum conj) {
		super(ChunkTypeEnum.conj, new ArrayList<Slot>());
		this.conj = conj;
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
