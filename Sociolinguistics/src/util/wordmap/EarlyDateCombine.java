package util.wordmap;

import java.time.Instant;

import util.data.Comment;

public class EarlyDateCombine extends Combinable {
	private final Instant time;
	public EarlyDateCombine() {
		this.time = Instant.MAX;
	}
	public EarlyDateCombine(Instant inst) {
		this.time = inst;
	}
	public EarlyDateCombine(String s) {
		this.time = Instant.ofEpochSecond(Long.parseLong(s));
	}
	public EarlyDateCombine(Comment c) {
		this.time = c.getTime();
	}
	public String toString() {
		return time.getEpochSecond()+"";
	}
	public Instant getTime() {
		return time;
	}
	@Override
	public Combinable combine(Combinable other) throws CombineException {
		if (!(other instanceof EarlyDateCombine)) {
			throw new CombineException();
		}
		EarlyDateCombine edc = (EarlyDateCombine) other;
		Instant newTime = this.time.isBefore(edc.time) ? this.time : edc.time;
		return new EarlyDateCombine(newTime);
	}
}
