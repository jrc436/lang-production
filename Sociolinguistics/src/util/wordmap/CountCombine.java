package util.wordmap;

import util.data.Comment;

public class CountCombine extends Combinable {
	private final int count;
	public CountCombine(int count) {
		this.count = count;
	}
	public CountCombine() {
		this.count = 1; //if we're making one, we've found one
	}
	public CountCombine(Comment c) {
		this.count = 1; //comment doesn't matter for counting!
	}
	public CountCombine(String s) {
		this.count = Integer.parseInt(s);
	}
	public String toString() {
		return count+"";
	}
	public int getCount() {
		return count;
	}
	@Override
	public Combinable combine(Combinable other) throws CombineException {
		if (!(other instanceof CountCombine)) {
			throw new CombineException();
		}
		CountCombine edc = (CountCombine) other;	
		return new CountCombine(edc.count + this.count);
	}
}
