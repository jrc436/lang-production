package util.collections;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DoubleKeyMap<E, V, K> extends HashMap<Pair<E, V>, K> {
	private static final long serialVersionUID = 2223800696872774323L;
	private final boolean symmetric;
	public DoubleKeyMap() {
		super();
		symmetric = false;
	}
	public DoubleKeyMap(boolean symmetric) {
		this.symmetric = symmetric;
	}
	public void put(E key1, V key2, K val) {
		if (symmetric) {
			super.put(new UnorderedPair<E, V>(key1, key2), val);
		}
		else {
			super.put(new OrderedPair<E, V>(key1, key2), val);
		}
	}
	public boolean containsKey(E key1, V key2) {
		if (symmetric) {
			return super.containsKey(new UnorderedPair<E, V>(key1, key2));
		}
		return super.containsKey(new OrderedPair<E, V>(key1, key2));
	}
	public K get(E key1, V key2) {
		if (symmetric) {
			return super.get(new UnorderedPair<E, V>(key1, key2));
		}
		return super.get(new OrderedPair<E, V>(key1, key2));
	}
	
	public List<E> getKeysetOne() {
		List<E> one = new ArrayList<E>();
		for (Pair<E, V> p : super.keySet()) {
			one.add(p.typeOne);
		}
		return one;
	}
	public List<V> getKeysetTwo() {
		List<V> two = new ArrayList<V>();
		for (Pair<E, V> p : super.keySet()) {
			two.add(p.typeTwo);
		}
		return two;
	}
	public Set<V> getFirstPairedKeys(E key1) {
		Set<V> pairedKeys = new HashSet<V>();
		for (Pair<E, V> p : super.keySet()) {
			if (p.typeOne.equals(key1)) {
				pairedKeys.add(p.typeTwo);
			}
		}
		return pairedKeys;
	}
	public Set<E> getSecondPairedKeys(V key2) {
		Set<E> pairedKeys = new HashSet<E>();
		for (Pair<E, V> p : super.keySet()) {
			if (p.typeTwo.equals(key2)) {
				pairedKeys.add(p.typeOne);
			}
		}
		return pairedKeys;
	}

}
