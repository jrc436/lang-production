package edu.psu.acs.lang.outputreader;

import java.util.ArrayList;
import java.util.List;

public class GarbleWord {
	private final IDWord data;
	private GarbleWord left;
	private GarbleWord right;
	public GarbleWord(IDWord data) {
		this.data = data;
	}
	protected IDWord getData() {
		return data;
	}
	@Override
	public String toString() {
		String retval = "";
		for (GarbleWord g : iterator()) {
			String addend = g.data.toString();
			if (g.data.equals(this.data)) {
				addend += "(root)";
			}
			retval += (addend + "->");
		}
		return retval.substring(0, retval.length()-2);
	}
	private List<GarbleWord> iterator() {
		List<GarbleWord> words = new ArrayList<GarbleWord>();
		GarbleWord find = this;
		while (find.getLeft() != null) {
			find = find.getLeft();
		}
		while (find != null) {
			words.add(find);
			find = find.getRight();
		}
		if (words.isEmpty()) {
			words.add(this);
		}
		return words;
	}
	@Override
	public boolean equals(Object other) {
		if (!(other instanceof GarbleWord)) {
			return false; 
		}
		GarbleWord o = (GarbleWord) other;
		List<GarbleWord> iter = iterator();
		List<GarbleWord> oiter = o.iterator();
		if (iter.size() != oiter.size()) {
			return false;
		}
		for (int i = 0; i < iter.size(); i++) {
			if (!iter.get(i).data.equals(oiter.get(i).data)) {
				return false;
			}
		}
		return true;
	}
	@Override
	public int hashCode() {
		int retval = 0;
		int iter = 1;
		for (GarbleWord gw : iterator()) {
			retval += (Math.pow(17, iter) * gw.data.hashCode());
		}
		return retval;
	}
	public GarbleWord getRight() {
		return right;
	}
	public GarbleWord getLeft() {
		return left;
	}
	public GarbleWord getFromData(IDWord id) {
		for (GarbleWord gw : iterator()) {
			if (gw.data.equals(id)) {
				return gw;
			}
		}
		return null;
	}
//	public GarbleWord getRoot(GarbleWord usedWord) {
//		GarbleWord gw = retrieveFragmentWord(usedWord);
//		if (gw != null) {
//			return gw;
//		}
//		return this;
//	}
//	public GarbleWord retrieveFragmentWord(GarbleWord g) {
//		for (GarbleWord gw : iterator()) {
//			if (gw.equals(g)) {
//				return gw;
//			}
//		}
//		return null;
//	}
	public boolean contains(GarbleWord g) {
		return iterator().contains(g);
	}
	public static void merge(GarbleWord left, GarbleWord right) {
		List<GarbleWord> lefter = left.iterator();
		List<GarbleWord> righter = right.iterator();
		lefter.get(lefter.size()-1).right = righter.get(0);
		righter.get(0).left = lefter.get(lefter.size()-1);
	}
//	public void addToTheRight(GarbleWord g) {
//		GarbleWord find = this;
//		while (find.getRight() != null) {
//			find = find.getRight();
//		}
//		if (find.right != null || g.left != null) {
//			System.err.println("Overwriting a reference on add to the right");
//			System.err.println("base: "+find.toString());
//			System.err.println("add: "+g.toString());
//		}
//		find.right = g;
//		g.left = find;
//	}
//	public void addToTheLeft(GarbleWord g) {
//		GarbleWord find = this;
//		while (find.getLeft() != null) {
//			find = find.getLeft();
//		}
//		if (find.left != null || g.right != null) {
//			System.err.println("Overwriting a reference on add to the left");
//			System.err.println("base: "+find.toString());
//			System.err.println("add: "+g.toString());
//		}
//		find.left = g;
//		g.right = find;
//	}
	public String getGarbleFragment() {
		String s = "";
		for (GarbleWord g : iterator()) {
			s += g.data.string + " ";
		}
		return s.substring(0, s.length()-1);
	}
}
