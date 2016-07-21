package edu.psu.acs.lang.output;

public class IDWord {
	private final int id;
	private final String string;
	public IDWord(String string, int id) {
		if (string == null) {
			throw new IllegalArgumentException("String cannot be null");
		}
		this.id = id;
		this.string = string;
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
		return string+"."+id;
	}
}
