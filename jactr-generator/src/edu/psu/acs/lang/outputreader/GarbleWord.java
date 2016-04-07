package edu.psu.acs.lang.outputreader;

public class GarbleWord {
	private final IDWord data;
	private GarbleWord left;
	private GarbleWord right;
	public GarbleWord(IDWord data) {
		this.data = data;
	}
	public boolean equals(Object other) {
		return (other instanceof GarbleWord) && ((GarbleWord)other).data.equals(this.data);
	}
	public int hashCode() {
		return data.hashCode();
	}
	public GarbleWord getRight() {
		return right;
	}
	public GarbleWord getLeft() {
		return left;
	}
	public void addToTheRight(GarbleWord g) {
		GarbleWord find = this;
		while (find.getRight() != null) {
			find = find.getRight();
		}
		find.right = g;
		g.left = find;
	}
	public void addToTheLeft(GarbleWord g) {
		GarbleWord find = this;
		while (find.getLeft() != null) {
			find = find.getLeft();
		}
		find.left = g;
		g.right = find;
	}
	public String getGarbleFragment() {
		GarbleWord find = this;
		while (this.getLeft() != null) {
			find = find.getLeft();
		}
		String fragment = find.data.string;
		while (find.getRight() != null) {
			find = find.getRight();
			fragment += " "+find.data.string;
		}
		return fragment;
	}
}
