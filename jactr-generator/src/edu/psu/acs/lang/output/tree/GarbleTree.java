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
	public List<String> getFragments() {
		List<String> frags = new ArrayList<String>();
		for (TreeNode tn : roots) {
			frags.add(tn.nodeString().trim());
		}
		return frags;
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
	public int computeCurMem(int offset) {
		//we have to add 2 because we removed from roots if we're going to recreate
		//if we aren't going to recreate, then we have to add two for the new node that's being added
		return this.roots.size() + offset; //not sure if htis is right yet...
	}
	private void addLine(IDWordPair iwp) {
		TreeNode leftSub = findSubTree(iwp.getLeft());
		TreeNode rightSub = findSubTree(iwp.getRight());
		int offset = 0;
		if (leftSub != null) {
			roots.remove(leftSub); //remove 1 from mem
			offset--;
		}
		else {
			leftSub = new GarbleNode(iwp.getLeft()); //add 1 to mem
			offset++;
		}
		if (rightSub != null) {
			roots.remove(rightSub); //remove 1 from mem
			offset--;
		}
		else {
			rightSub = new GarbleNode(iwp.getRight()); //add 1 to mem
			offset++;
		}
		this.maxMem = Math.max(maxMem, computeCurMem(offset));
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
	public double computeUnweightedBranchFactor() {
		//it is a left-branch if right is bigger, a right-branch if left is bigger, and neutral if they are the same size
		int totalRight = 0;
		int totalLeft = 0;
		int totalBranches = 0;
		for (TreeNode root : roots) {
//			int rootLeftBranches = 0;
//			int rootRightBranches = 0;
			Queue<TreeNode> subtrees = new LinkedList<TreeNode>();
			subtrees.offer(root);
			while (!subtrees.isEmpty()) {
				TreeNode cur = subtrees.poll();
				if (cur instanceof GarbleNode) {
					continue;
				}
				subtrees.offer(cur.getLeft());
				subtrees.offer(cur.getRight());
				int left = cur.countLeftLeaves();
				int right = cur.countRightLeaves();
				if (left > right) {
					//leftTotal
					totalRight++;
				}
				else if (right > left) {
					totalLeft++;
				}
				totalBranches++;
				//int leftSize = cur.countLeftLeaves();
				//int rightSize = cur.countRightLeaves();
			}
			//sum += ((double) leftTotal) / ((double) rightTotal);
			//totalLeft += leftTotal;
			//allRightBranches += rootRightBranches;
		}
		return ((double) totalRight) / ((double) totalBranches);
	}
	//the weighted branch factor has to do with the nodes in the tree. A higher score represents something that is more
	//right-branching
	public double computeWeightedBranchFactor() {
		int totalLeft = 0;
		int totalRight = 0;
		for (TreeNode root : roots) {
			Queue<TreeNode> subtrees = new LinkedList<TreeNode>();
			subtrees.offer(root);
			while (!subtrees.isEmpty()) {
				TreeNode cur = subtrees.poll();
				if (cur instanceof GarbleNode) {
					continue;
				}
				subtrees.offer(cur.getLeft());
				subtrees.offer(cur.getRight());
				totalLeft += cur.countLeftLeaves();
				totalRight += cur.countRightLeaves();
			}
		}
		return ((double) totalLeft) / ((double) totalRight);
	}
}
