package edu.psu.acs.lang.eval;

import edu.psu.acs.lang.declarative.Word;
import edu.psu.acs.lang.production.SyntaxRule;

public class IDWord {
	private final int id;
	private final String string;
	public IDWord(String string, int id) {
		if (string == null) {
			throw new IllegalArgumentException("String cannot be null");
		}
		this.id = id;
		this.string = Word.reverseNameConst(string);
	}
	@Override
	public boolean equals(Object other) {
		if (other == null || !other.getClass().equals(this.getClass())) {
			return false;
		}
		IDWord idw = (IDWord) other;
		return idw.id == this.id && idw.string.equals(this.string);
	}
	@Override
	public int hashCode() {
		return string.hashCode() + 19 * id;
	}
	public String getString() {
		return string;
	}
	@Override
	public String toString() {
		return Word.getNameConst(string)+SyntaxRule.wordSep+id;
	}
	public static IDWord fromString(String s) {
		String[] parts = s.split(SyntaxRule.wordSep);
		if (parts.length != 2) {
			System.err.println(s);
			throw new IllegalArgumentException("This isn't an IDWord");
		}
		return new IDWord(parts[0], Integer.parseInt(parts[1]));
	}
}
