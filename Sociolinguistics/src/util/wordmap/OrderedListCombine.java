//package util.wordmap;
//
//import java.util.ArrayList;
//import java.util.Comparator;
//import java.util.List;
//
//public abstract class OrderedListCombine extends Combinable {
//	private final Comparator<String> sorter;
//	private final List<String> sorted;
////	public OrderedListCombine() {
////		this(dateSorter());
////	}
//	public OrderedListCombine(Comparator<String> sorter) {
//		this.sorter = sorter;
//		sorted = new ArrayList<String>();
//	}
//	@Override
//	public Combinable combine(Combinable other) throws CombineException {
//		if (!(other instanceof OrderedListCombine)) {
//			throw new CombineException();
//		}
//		OrderedListCombine olc = (OrderedListCombine) other;
//		//Instant newTime = this.time.isAfter(edc.time) ? this.time : edc.time;
//		//return new LateDateCombine(newTime);
//		return olc;
//	}
//	
//	
//}
