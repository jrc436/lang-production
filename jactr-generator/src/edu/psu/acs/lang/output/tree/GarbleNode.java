package edu.psu.acs.lang.output.tree;

import edu.psu.acs.lang.output.IDWord;

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
}
