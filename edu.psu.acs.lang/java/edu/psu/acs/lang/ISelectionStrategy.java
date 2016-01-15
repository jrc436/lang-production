package edu.psu.acs.lang;

import java.util.Set;

import edu.psu.acs.lang.IDependencyGraph.IDependencyNode;
/**
 * Sometimes, there are multiple choices available. So how does one choose, exactly?
 * @author jrc
 *
 * @param <E>
 */
@FunctionalInterface
public interface ISelectionStrategy<E> {
	public IDependencyNode<E> select(Set<IDependencyNode<E>> choices);
}
