package edu.psu.acs.lang.util;

import java.util.HashSet;
import java.util.Set;

import edu.psu.acs.lang.tracing.Equivocable;
import edu.psu.acs.lang.tracing.IDependencyGraph;

/**
 * @author jrc
 * Not the most efficient methods. Just implemented quickly
 * @param <E>
 */
public class DependencyGraph<E extends Equivocable> implements IDependencyGraph<E> {
	private Set<IDependencyNode<E>> nodes; 
	public DependencyGraph() {
		nodes = new HashSet<IDependencyNode<E>>();
	}
	public IDependencyNode<E> addNode(E data) {
		IDependencyNode<E> dat = new DependencyNode<E>(data);
		nodes.add(dat);
		return dat;
	}
	public IDependencyNode<E> addNode(E data, Set<IDependencyNode<E>> dependencies) {
		IDependencyNode<E> dat = new DependencyNode<E>(data, dependencies);
		nodes.add(dat);
		return dat;
	}
	private Set<IDependencyNode<E>> getNodeByData(E dat) {
		Set<IDependencyNode<E>> matches = new HashSet<IDependencyNode<E>>();
		for (IDependencyNode<E> node : nodes) {
			if (node.getData().equals(dat)) {
				matches.add(node);
			}
		}
		return matches;
	}
	private Set<IDependencyNode<E>> getNodesByValidation(Object[] validators) {
		Set<IDependencyNode<E>> matches = new HashSet<IDependencyNode<E>>();
		for (IDependencyNode<E> node : nodes) {
			if (node.getData().validate(validators)) {
				matches.add(node);
			}
		}
		return matches;
	}
	@Override
	public IDependencyNode<E> getFreeEquivNode(Object[] validators) {
		Set<IDependencyNode<E>> matches = getNodesByValidation(validators);
		for (IDependencyNode<E> n : matches) {
			if (!n.hasDependencies()) {
				return n;
			}
		}
		return null;
	}
	@Override
	public IDependencyNode<E> getFreeEqualNode(E data) {
		Set<IDependencyNode<E>> matches = getNodeByData(data);
		for (IDependencyNode<E> n : matches) {
			if (!n.hasDependencies()) {
				return n;
			}
		}
		return null;
	}
	public IDependencyNode<E> popNode(IDependencyNode<E> node) {
		if (node.hasDependencies()) {
			throw new IllegalArgumentException("Nodes must be free before they can be popped. Pop their dependencies first.");
		}
		for (IDependencyNode<E> rn : nodes) {
			rn.clearDependency(node);
		}
		nodes.remove(node);
		return node;
	}
	class DependencyNode<F extends E> implements IDependencyNode<F> {
		private F data;
		private Set<IDependencyNode<F>> dependentOn;
		private DependencyNode(F data) {
			this.data = data;
			dependentOn = new HashSet<IDependencyNode<F>>();
		}
		private DependencyNode(F data, Set<IDependencyNode<F>> dependentOn) {
			this(data);
			this.dependentOn = dependentOn;
		}
		@Override
		public boolean addDependency(IDependencyNode<F> newDep) {
			return dependentOn.add(newDep);
		}
		@Override
		public boolean hasDependencies() {
			return !dependentOn.isEmpty();
		}
		@Override
		public boolean clearDependency(IDependencyNode<F> oldDep) {
			return dependentOn.remove(oldDep);
		}
		@Override
		public F getData() {
			return data;
		}
	}


	
}
