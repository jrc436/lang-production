package edu.psu.acs.lang.declarative;

import java.util.ArrayList;

import edu.psu.acs.lang.declarative.chunk.ChunkStore;
import edu.psu.acs.lang.declarative.chunk.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.slot.Slot;


public class Word extends ChunkStore {
	private String word;
	public Word(String word) {
		super(ChunkTypeEnum.word, new ArrayList<Slot>());
		this.word = word;
	}
	private static final String nameConst = "word";
	public String toString() {
		return getNameConst(word);
	}
	public static String reverseConst(String string, String split) {
		String[] parts = string.split("-");
		if (parts.length < 2 || !parts[parts.length-1].equals(nameConst)) {
			System.err.println(string);
			throw new IllegalArgumentException("This doesn't follow the correct specification");
		}
		String retval = "";
		for (int i = 0; i < parts.length-1; i++) {
			retval += parts[i] + "-";
		}
		return retval.substring(0, retval.length()-1);
	}
	public static String reverseNameConst(String wordc) {
		return reverseConst(wordc, nameConst);
	}
	public static String getNameConst(String word) {
		return word+"-" + nameConst;
	}
	public static Word factory() {
		return new Word(null);
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
