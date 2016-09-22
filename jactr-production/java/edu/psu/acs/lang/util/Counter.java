package edu.psu.acs.lang.util;

public class Counter<E> implements Comparable<Counter<E>> {
	private Integer count;
	private final E data;
	public Counter(E data, int count) {
		this.data = data;
		this.count = count;
	}
	public void addToCount(int add) {
		count += add;
	}
	public void plusplus() {
		count++;
	}
	@Override
	public int compareTo(Counter<E> arg0) {
		return this.count.compareTo(arg0.count);
	}
	public E getData() {
		return data;
	}
	public String toString() {
		return data.toString()+":"+count;
	}
}
