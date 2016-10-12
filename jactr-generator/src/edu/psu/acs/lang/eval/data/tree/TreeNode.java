package edu.psu.acs.lang.eval.data.tree;

import edu.psu.acs.lang.output.IDWord;

public interface TreeNode {
	public TreeNode getLeft();
	public TreeNode getRight();
	public int countLeftLeaves();
	public int countRightLeaves();
	public int countAllLeaves();
	public boolean contains(IDWord word);
	public String nodeString();
	public int getSubtreeSize();
	public String uglyPrint();
	
	public static TreeNode fromString(String s) {
		if (s.isEmpty() || !s.contains("[") || !s.contains("]")) {
			System.err.println(s);
			throw new IllegalArgumentException("A node must have brackets");
		}
		if (s.indexOf('[') != s.lastIndexOf('[')) {
			return DummyNode.fromString(s);
		}
		else {
			return GarbleNode.fromString(s);
		}
	}
}
