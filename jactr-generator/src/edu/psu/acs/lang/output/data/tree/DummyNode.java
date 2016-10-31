package edu.psu.acs.lang.output.data.tree;

import edu.psu.acs.lang.output.data.raw.IDWord;

public class DummyNode implements TreeNode {
	private final TreeNode left;
	private final TreeNode right;
	public DummyNode(TreeNode left, TreeNode right) {
		this.left = left;
		this.right = right;
	}
	@Override
	public TreeNode getLeft() {
		return left;
	}
	@Override
	public TreeNode getRight() {
		return right;
	}
	@Override
	public int countLeftLeaves() {
		return left.countAllLeaves();
	}
	@Override
	public int countRightLeaves() {
		return right.countAllLeaves();
	}
	@Override
	public boolean contains(IDWord word) {
		return left.contains(word) || right.contains(word);
	}
	public String nodeString() {
		return left.nodeString().trim() + " " + right.nodeString().trim();
	}
	@Override
	public int countAllLeaves() {
		return left.countAllLeaves() + right.countAllLeaves();
	}
	@Override
	public int getSubtreeSize() {
		return left.getSubtreeSize() + right.getSubtreeSize();
	}
	public String uglyPrint() {
		return "()";
	}
	public int hashCode() {
		return 17 * left.hashCode() + 13 * right.hashCode();
	}
	public String toString() {
		return "["+left.toString()+right.toString()+"]";
	}
	public static DummyNode fromString(String inp) {
		String clean = stripBrackets(inp);
		int bracketPolice = 0;
		if (clean.charAt(0) != '[') {
			System.err.println(clean);
			throw new IllegalArgumentException(inp+" is malformed");
		}
		int indexOf = -1;
		for (int i = 0; i < clean.length(); i++) {
			if (clean.charAt(i) == '[') {
				bracketPolice++;
			}
			else if (clean.charAt(i) == ']') {
				bracketPolice--;
			}
			if (bracketPolice == 0) {
				indexOf = i;
				break;
			}
		}
		if (indexOf+1 == clean.length() || 0 == indexOf+1) {
			throw new IllegalArgumentException(inp+" is probably a GarbleNode");
		}
		return new DummyNode(TreeNode.fromString(clean.substring(0, indexOf+1)), TreeNode.fromString(clean.substring(indexOf+1, clean.length())));
	}
	public static String stripBrackets(String s) {
		if (s.charAt(0) != '[' || s.charAt(s.length()-1) != ']') {
			System.err.println("Could be wrong type of node");
			System.err.println(s);
			throw new IllegalArgumentException("Can't strip brackets that aren't there");
		}
		return s.substring(1, s.length()-1);
	}
	public boolean equals(Object other) {
		if (!(other instanceof DummyNode)) {
			return false;
		}
		DummyNode dn = (DummyNode) other;
		return dn.left.equals(this.left) && dn.right.equals(this.right);
	}
}
