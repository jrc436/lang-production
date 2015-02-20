package ar.grammarcreator;

import java.util.ArrayList;


//for use with morph.xml, mostly just a struct
public class Entry {
	private final String word;
	private final String pos;
	private final String stem;
	private final ArrayList<String> macros;
	public Entry(String word, String pos) {
		macros = new ArrayList<String>();
		this.word = word;
		this.pos = pos;
		this.stem = "";
	}
	public Entry(String word, String pos, String stem) {
		macros = new ArrayList<String>();
		this.word = word;
		this.pos = pos;
		this.stem = stem;
	}
	public String getMacros() {
		String out = "";
		for (String s : macros) {
			out += ("@"+s+" ");
		}
		out.trim();
		return out;
	}
	public String getStem() {
		return stem;
	}
	public String getPos() {
		return pos;
	}
	public String getWord() {
		return word;
	}
}
