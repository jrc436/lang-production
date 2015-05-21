package util;

@FunctionalInterface
public interface CollectionContains<E> {
	public boolean contains(E o);
}
