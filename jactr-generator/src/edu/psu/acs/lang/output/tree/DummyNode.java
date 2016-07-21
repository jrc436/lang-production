package edu.psu.acs.lang.output.tree;

import edu.psu.acs.lang.output.IDWord;

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
}
