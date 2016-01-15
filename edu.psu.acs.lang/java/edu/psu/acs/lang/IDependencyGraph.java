package edu.psu.acs.lang;

import java.util.Set;



public interface IDependencyGraph<E> {
	public IDependencyNode<E> getFreeEqualNode(E data);
	public IDependencyNode<E> getFreeEquivNode(Object[] validators);
	public IDependencyNode<E> addNode(E data, Set<IDependencyNode<E>> dependencies);
	public IDependencyNode<E> popNode(IDependencyNode<E> node);
	//public IDependencyNode<E> getTopNode(ISelectionStrategy<E> strat);
	interface IDependencyNode<E> {
		boolean addDependency(IDependencyNode<E> newDep);
		boolean hasDependencies();
		boolean clearDependency(IDependencyNode<E> oldDep);
		E getData();
	}
}
