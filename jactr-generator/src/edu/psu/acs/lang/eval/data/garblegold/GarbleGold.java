package edu.psu.acs.lang.eval.data.garblegold;

import edu.psu.acs.lang.output.tree.GarbleTree;

public class GarbleGold {
	private GarbleTree garbleSentence;
	private String goldSentence;
	public GarbleTree getGarble() {
		return garbleSentence;
	}
	public String getGold() {
		return goldSentence;
	}
	public GarbleGold(GarbleTree gas, String gs) {
		if (gas == null && gs == null) {
			throw new IllegalArgumentException("Completely null Garblegolds aren't allowed");
		}
		this.garbleSentence = gas;
		this.goldSentence = gs;
	}
	public void combine(GarbleGold other) {
		if (this.garbleSentence == null && other.garbleSentence != null) {
			this.garbleSentence = other.garbleSentence;
		}
		if (this.goldSentence == null && other.goldSentence != null) {
			this.goldSentence = other.goldSentence;
		}
		//throw new IllegalArgumentException("Garblegold: "+other+" has nothing to offer: "+this);
	}
	public boolean complete() {
		return garbleSentence != null && goldSentence != null;
	}
	public boolean equals(Object other) {
		if (!(other instanceof GarbleGold)) {
			return false;
		}		
		GarbleGold gs = (GarbleGold) other;
		if (this.garbleSentence == null && gs.garbleSentence == null && this.goldSentence.equals(gs.goldSentence)) {
			return true;
		}
		else if (this.goldSentence == null && gs.goldSentence == null && this.garbleSentence.equals(gs.garbleSentence)) {
			return true;
		}
		else if (!this.complete() || !gs.complete()) {
			return false;
		}
		return garbleSentence.equals(gs.garbleSentence) && goldSentence.equals(gs.goldSentence); 
	}
	public int hashCode() {
		int part1 = garbleSentence == null ? 0 : garbleSentence.hashCode() * 7;
		int part2 = goldSentence == null ? 0 : goldSentence.hashCode() * 13;
		return part1 + part2;
	}
	private static final String delim = "::num:garb:gold::";
	public String toString() {
		String garbPart = garbleSentence != null ? garbleSentence.toString() : "";
		String goldPart = goldSentence != null ? goldSentence : "";
		return garbPart + delim + goldPart;
	}
	public static GarbleGold fromString(String s) {
		String[] parts = s.split(delim);
		if (parts.length != 2) {
			throw new IllegalArgumentException("This string doesn't contain a GarbleGold");
		}
		GarbleTree gt = parts[0].isEmpty() ? new GarbleTree() : GarbleTree.fromString(parts[0]);
		return new GarbleGold(gt, parts[1]);
	}
}
