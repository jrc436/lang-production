package edu.psu.acs.lang.outputreader;

public class IDWord {
	protected final int id;
	protected final String string;
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
	@Override
	public String toString() {
		return string+"."+id;
	}
}
