package edu.psu.acs.lprod.util;

import java.util.Collection;

public class ListMath {
	public static int sumI(Collection<Integer> collection) {
		int sum = 0;
		for (int el : collection) {
			sum += el;
		}
		return sum;
	}
	public static double avgI(Collection<Integer> collection) {
		return ((double) sumI(collection)) / ((double) collection.size()); 
	}
	public static double sumD(Collection<Double> collection) {
		double sum = 0;
		for (double el : collection) {
			sum += el;
		}
		return sum;
	}
	public static double avgD(Collection<Double> collection) {
		return sumD(collection) / ((double) collection.size());
	}
}
