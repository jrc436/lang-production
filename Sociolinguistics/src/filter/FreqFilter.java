package filter;

import java.util.Map.Entry;

import util.wordmap.Combinable;
import util.wordmap.CountCombine;
import util.wordmap.WordMap;

public class FreqFilter extends WordMapFilter {
	//private static final double lowerboundFreq = 0.0001;
	//private static final double upperboundFreq = 0.005;
	private static final int min = 100;
	private static final int max = Integer.MAX_VALUE;
//	private int getMapFreqSum(WordMap wm) {
//		int sum = 0;
//		for (String s : wm.keySet()) {
//			sum += ((CountCombine) wm.getBy(s, CountCombine.class)).getCount();
//		}
//		return sum;
//	}
//	private static boolean isBetween(double lowerBound, double upperBound, int count, int total) {
//		double rate = ((double)count) / ((double) total);
//		return rate >= lowerBound && rate <= upperBound;
//	}
	private static boolean isBetween(int lowerBound, int upperBound, int count) {
//		double rate = ((double)count) / ((double) total);
//		return rate >= lowerBound && rate <= upperBound;
		return count >= lowerBound && count <= upperBound;
	}
//	}
	@Override
	protected IWordMapFilter createFilter(WordMap wm) {
		return new IWordMapFilter() {
//			int freqSum = getMapFreqSum(wm);
//			@Override
//			public boolean goodEntry(Entry<String, Combinable> entry) {
//				return isBetween(lowerboundFreq, upperboundFreq, ((CountCombine) wm.getBy(entry.getKey(), CountCombine.class)).getCount(), freqSum);
//			}
			public boolean goodEntry(Entry<String, Combinable> entry) {
				return isBetween(min, max, ((CountCombine) wm.getBy(entry.getKey(), CountCombine.class)).getCount());
			}
//				return isBetween(lowerboundFreq, upperboundFreq, ((CountCombine) wm.getBy(entry.getKey(), CountCombine.class)).getCount(), freqSum);
//			}
		};
	}
}
