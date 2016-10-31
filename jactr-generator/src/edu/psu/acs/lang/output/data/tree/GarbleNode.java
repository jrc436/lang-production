package edu.psu.acs.lang.output.data.tree;

import edu.psu.acs.lang.output.data.raw.IDWord;

public class GarbleNode implements TreeNode {
	private final IDWord data;
//	private GarbleNode left;
//	private GarbleNode right;
	public GarbleNode(IDWord data) {
		this.data = data;
	}
	protected IDWord getData() {
		return data;
	}
	@Override
	public TreeNode getLeft() {
		return null;
	}
	@Override
	public TreeNode getRight() {
		return null;
	}
	@Override
	public int countLeftLeaves() {
		return 1;
	}
	@Override
	public int countRightLeaves() {
		return 1;
	}
	@Override
	public boolean contains(IDWord word) {
		return data.equals(word);
	}
	@Override
	public String nodeString() {
		return data.getString();
	}
	@Override
	public int countAllLeaves() {
		return 1;
	}
	@Override
	public int getSubtreeSize() {
		return 1;
	}
	public String uglyPrint() {
		return nodeString();
	}
	@Override
	public int hashCode() {
		return data.hashCode();
	}
	@Override
	public boolean equals(Object other) {
		if (!(other instanceof GarbleNode)) {
			return false;
		}
		GarbleNode gn = (GarbleNode) other;
		return this.data.equals(gn.data);
	}
	public static GarbleNode fromString(String s) {
		return new GarbleNode(IDWord.fromString(DummyNode.stripBrackets(s)));
	}
	public String toString() {
		return "["+data.toString()+"]";
	}
}
