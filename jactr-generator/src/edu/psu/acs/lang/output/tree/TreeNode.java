package edu.psu.acs.lang.output.tree;

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
}
