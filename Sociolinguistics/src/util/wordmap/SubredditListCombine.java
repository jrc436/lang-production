package util.wordmap;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import util.data.Comment;

public class SubredditListCombine extends Combinable {
	private final Map<String, Instant> subredditMap;
	//private final List<String> subredditOrdering;
	public SubredditListCombine() {
		this.subredditMap = new HashMap<String, Instant>();
		//this.subredditOrdering = new ArrayList<String>();
	}
	public SubredditListCombine(Comment c) {
		this();
		addMin(c.getField("subreddit"), c.getTime());
	}
	private void addMin(String sub, Instant time) {
		if (!subredditMap.containsKey(sub)) {
			subredditMap.put(sub, time);
			return;
		}
		Instant lowest = subredditMap.get(sub).compareTo(time) < 0 ? subredditMap.get(sub) : time;
		subredditMap.put(sub, lowest);
	}
	private SubredditListCombine(Map<String, Instant> map) {
		this.subredditMap = new HashMap<String, Instant>(map);
		//this.subredditOrdering = new ArrayList<String>();
	}
	public List<String> produceOrdering() {
		List<String> ordered = new ArrayList<String>();
		for (String key : subredditMap.keySet()) {
			boolean added = false;
			for (int i = 0; i < ordered.size(); i++) {
				if (compare(key, ordered.get(i)) < 0) {
					ordered.add(i, key);
				}
			}
			if (!added) {
				ordered.add(key);
			}
		}
		return ordered;
	}
	public String toString() {
		List<String> order = produceOrdering();
		String toret = "";
		for (String sub : order) {
			toret += sub + ",";
		}
		return toret.substring(0, toret.length()-1);
	}
	//return -1 if arg0 had occurred before, 0 if at the same time, 1 if after 
	private int compare(String arg0, String arg1) {
		if (!subredditMap.containsKey(arg0) || !subredditMap.containsKey(arg1)) {
			throw new IllegalArgumentException("both arguments must be in the map to produce an ordering");
		}
		return subredditMap.get(arg0).compareTo(subredditMap.get(arg1));
	}

	public Comparator<String> dateSorter() {
		return new Comparator<String>() {	
			@Override
			public int compare(String arg0, String arg1) {
				return subredditMap.get(arg0).compareTo(subredditMap.get(arg1));
			}
		};
	}

	@Override
	public Combinable combine(Combinable other) throws CombineException {
		if (!(other instanceof SubredditListCombine)) {
			throw new CombineException();
		}
		SubredditListCombine olc = (SubredditListCombine) other;
		//Instant newTime = this.time.isAfter(edc.time) ? this.time : edc.time;
		//return new LateDateCombine(newTime);
		combine(subredditMap, olc.subredditMap);
		return new SubredditListCombine(subredditMap);
	}
	private static void combine(Map<String, Instant> first, Map<String, Instant> second) {
		for (Map.Entry<String,Instant> entry : first.entrySet()) {
			if (second.containsKey(entry.getKey()) && second.get(entry.getKey()).compareTo(entry.getValue()) < 0) {
				entry.setValue(second.get(entry.getKey()));
			}
		}
		for (Map.Entry<String, Instant> entry : second.entrySet()) {
			if (!first.containsKey(entry.getKey())) {
				first.put(entry.getKey(), entry.getValue());
			}
		}
	}
//	@Override
//	public String clsString() {
//		return "slc";
//	}
}
