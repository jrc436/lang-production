package edu.psu.acs.lang.eval;

import edu.psu.acs.lang.production.SyntaxRule;

public class IDWordPair {
	private final IDWord left;
	private final IDWord right;
	private final boolean swaparoo;
	public IDWordPair(String garble) {
		String[] garbles = null;
		boolean swaparoo = false;
		if (garble.contains(SyntaxRule.leftOf)) {
			garbles = garble.split(SyntaxRule.leftOf);
			swaparoo = true;
		}
		else if (garble.contains(SyntaxRule.rightOf)) {
			garbles = garble.split(SyntaxRule.rightOf);
		}
		else {
			throw new IllegalArgumentException("GarbleLists must take the garble produced by model outputs, which should contain a SyntaxRule.leftOf or rightOf");
		}
		this.swaparoo = swaparoo;
		String[] leftParts = garbles[0].split(SyntaxRule.wordSep);
		IDWord leftdat = new IDWord(leftParts[0].trim(), Integer.parseInt(leftParts[1].trim()));
		String[] rightParts = garbles[1].split(SyntaxRule.wordSep);
		IDWord rightdat = new IDWord(rightParts[0].trim(), Integer.parseInt(rightParts[1].trim()));
		if (swaparoo) {
			IDWord tmp = leftdat;
			leftdat = rightdat;
			rightdat = tmp;
		}
		this.left = leftdat;
		this.right = rightdat;
	}
	public IDWord getLeft() {
		return left;
	}
	public IDWord getRight() {
		return right;
	}
	public String toString() {
		String sep = swaparoo ? SyntaxRule.leftOf : SyntaxRule.rightOf;
		String rightPart = swaparoo ? left.toString() : right.toString();
		String leftPart = swaparoo ? right.toString() : left.toString();
		return leftPart + sep + rightPart;
	}
	public int hashCode() {
		int rightTimes = swaparoo ? 43 : 29;
		return left.hashCode() + right.hashCode() * rightTimes;
	}
	public boolean equals(Object other) {
		if (!(other instanceof IDWordPair)) {
			return false;
		}
		IDWordPair iwp = (IDWordPair) other;
		return this.left.equals(iwp.left) && this.right.equals(iwp.right) && this.swaparoo == iwp.swaparoo;
	}
}
