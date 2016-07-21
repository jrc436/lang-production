package edu.psu.acs.lang.output.tree;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;

import edu.psu.acs.lang.output.IDWord;
import edu.psu.acs.lang.output.IDWordPair;
import edu.psu.acs.lang.output.OutputSentence;

public class GarbleTree {
	private final List<TreeNode> roots;
	private int maxMem;
	public GarbleTree() {
		this.roots = new ArrayList<TreeNode>();
		this.maxMem = 0;
	}
	public int getMaxMem() {
		return maxMem;
	}
	public GarbleTree(OutputSentence os) {
		this();
		for (IDWordPair iwp : os) {
			addLine(iwp);
		}
	}
	public double getMemRatio() {
		return ((double) size()) / ((double) getMaxMem());
	} 
	public int size() {
		int sum = 0;
		for (TreeNode root : roots) {
			sum += root.getSubtreeSize();
		}
		return sum;
	}
	public int computeCurMem() {
		//we have to add 2 because we removed from roots if we're going to recreate
		//if we aren't going to recreate, then we have to add two for the new node that's being added
		return this.roots.size() + 2;
	}
	public void addLine(IDWordPair iwp) {
		TreeNode leftSub = findSubTree(iwp.getLeft());
		TreeNode rightSub = findSubTree(iwp.getRight());
		if (leftSub != null) {
			roots.remove(leftSub);
		}
		else {
			leftSub = new GarbleNode(iwp.getLeft());
		}
		if (rightSub != null) {
			roots.remove(rightSub);
		}
		else {
			rightSub = new GarbleNode(iwp.getRight());
		}
		this.maxMem = Math.max(maxMem, computeCurMem());
		roots.add(new DummyNode(leftSub, rightSub));
	}
	private TreeNode findSubTree(IDWord idw) {
		for (TreeNode root : roots) {
			if (root.contains(idw)) {
				return root;
			}
		}
		return null;
	}
	public double getAverageBranchFactor() {
		//double sum = 0.0;
		int totalLeft = 0;
		int totalRight = 0;
		for (TreeNode root : roots) {
			int leftTotal = 0;
			int rightTotal = 0;
			Queue<TreeNode> subtrees = new LinkedList<TreeNode>();
			subtrees.offer(root);
			while (!subtrees.isEmpty()) {
				TreeNode cur = subtrees.poll();
				if (cur instanceof GarbleNode) {
					continue;
				}
				subtrees.offer(cur.getLeft());
				subtrees.offer(cur.getRight());
				leftTotal += cur.countLeftLeaves();
				rightTotal += cur.countRightLeaves();
			}
			//sum += ((double) leftTotal) / ((double) rightTotal);
			totalLeft += leftTotal;
			totalRight += rightTotal;
		}
		return ((double) totalLeft) / ((double) totalRight);
		//return sum / roots.size();
	}
}
