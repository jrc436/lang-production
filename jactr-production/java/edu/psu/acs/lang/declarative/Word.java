package edu.psu.acs.lang.declarative;

import java.util.ArrayList;


public class Word extends ChunkStore implements SlotValue {
	private String word;
	public Word(String word) {
		super(ChunkTypeEnum.word, new ArrayList<Slot>());
		this.word = word;
	}
	public String toString() {
		return word;
	}
	@Override
	public boolean equals(Object other) {
		if (other == null || !(other instanceof Word)) {
			return false;
		}
		return other.toString().equals(this.toString());
	}
	public int hashCode() {
		return this.toString().hashCode();
	}
}
