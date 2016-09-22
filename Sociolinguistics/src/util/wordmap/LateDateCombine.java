package util.wordmap;

import java.time.Instant;

import util.data.Comment;

public class LateDateCombine extends Combinable {
	private final Instant time;
	public LateDateCombine() {
		this.time = Instant.MIN;
	}
	public LateDateCombine(Instant inst) {
		this.time = inst;
	}
	public LateDateCombine(Comment c) {
		this.time = c.getTime();
	}
	public String toString() {
		return time.toString();
	}
	@Override
	public Combinable combine(Combinable other) throws CombineException {
		if (!(other instanceof LateDateCombine)) {
			throw new CombineException();
		}
		LateDateCombine edc = (LateDateCombine) other;
		Instant newTime = this.time.isAfter(edc.time) ? this.time : edc.time;
		return new LateDateCombine(newTime);
	}
	@Override
	public String clsString() {
		return "ldc";
	}

}