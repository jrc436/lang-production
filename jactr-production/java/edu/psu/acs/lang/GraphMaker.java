package edu.psu.acs.lang;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.psu.acs.lang.IDependencyGraph.IDependencyNode;
import edu.psu.acs.lang.util.DependencyGraph;
import edu.psu.acs.lang.util.NodeParser;
import edu.psu.acs.lang.util.ParseNode;

public class GraphMaker {
	//now that we've theoretically got everything sorted nice and neatly into RuleNodes and LexNodes via NodeParser, 
	//we actually just want RuleNodes, so that we can compare rules firing to those RuleNodes. In this case, a RuleNode whose left and right values
	//are LexNodes instead of RuleNodes win the day. So our "root nodes" in the dependency graph have two lexnodes. RuleNodes are dependent on RuleNodes
	//they contain in their left or right values. The light at the end of the tunnel here is, of course, that each rule node is only dependent on at most
	//two other nodes... so basically, we want to iterate through the node parser, at every point create dependency nodes, add the ones that are rules
	//to their dependencies, and then recurse
	public static List<IDependencyGraph<RuleNode>> make(NodeParser n) {
		List<ParseNode> rn = n.getTops();
		List<IDependencyGraph<RuleNode>> graphs = new ArrayList<IDependencyGraph<RuleNode>>();
		for (ParseNode pn : rn) {
			RuleNode r = (RuleNode) pn;
			IDependencyGraph<RuleNode> graph = new DependencyGraph<RuleNode>();
			addNodeWithDependencies(graph, r);
			graphs.add(graph);
		}
		return graphs;
	}
	private static IDependencyNode<RuleNode> addNodeWithDependencies(IDependencyGraph<RuleNode> graph, RuleNode node) {
		//a parse node has two children, which of course, might also have children. its children are its dependencies, who of course, might have dependencies
		//in their own children, so...
	    Set<IDependencyNode<RuleNode>> dependencies = new HashSet<IDependencyNode<RuleNode>>();
		if (node.getLeftChild() instanceof RuleNode) {
			RuleNode left = (RuleNode) node.getLeftChild();
			dependencies.add(addNodeWithDependencies(graph, left));
	    }
		if (node.getRightChild() instanceof RuleNode) {
			RuleNode right = (RuleNode) node.getRightChild();
			dependencies.add(addNodeWithDependencies(graph, right));
		}
		return graph.addNode(node, dependencies);
	}
}
