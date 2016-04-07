package edu.psu.acs.lang.outputreader;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.production.SyntaxRule;

public class GarbleFragments {
	private final ArrayList<GarbleWord> fragments; //should only include root words.
	public GarbleFragments() {
		this.fragments = new ArrayList<GarbleWord>();
	}
	//may have to be updated;
	public static String getGarbleFromOutputLine(String outputLine) {
		outputLine = outputLine.substring(outputLine.indexOf(')'));
		return outputLine.trim();
	}
	public void addGarble(String garble) {
		//more or less our process:
		//determine if this is a combination of root fragments: both left and right are root fragments
		//determine if this is a new root fragment: neither left not right are root fragments
		//else, add to the left or right as necessary
		
		//if left was just added, that means RIGHT could be a root fragment. If left is also a root fragment, that means
		//it's a root fragment combination. If RIGHT is not a root fragment, that means it's a new root fragment.
		boolean leftJustAdded = false; 
		String[] garbles = null;
		if (garble.contains(SyntaxRule.leftOf)) {
			garbles = garble.split(SyntaxRule.leftOf);
			leftJustAdded = true;
		}
		else if (garble.contains(SyntaxRule.rightOf)) {
			garbles = garble.split(SyntaxRule.rightOf);
		}
		else {
			throw new IllegalArgumentException("GarbleLists must take the garble produced by model outputs, which should contain a SyntaxRule.leftOf or rightOf");
		}
		String[] leftParts = garbles[0].split(SyntaxRule.wordSep);
		GarbleWord left = new GarbleWord(new IDWord(leftParts[0].trim(), Integer.parseInt(leftParts[1].trim())));
		String[] rightParts = garbles[1].split(SyntaxRule.wordSep);
		GarbleWord right = new GarbleWord(new IDWord(rightParts[0].trim(), Integer.parseInt(rightParts[1].trim())));
		if (leftJustAdded) {
			if (!fragments.contains(right)) {
				fragments.add(right); //adding a new fragment
			}
			else if (fragments.contains(left)) {
				fragments.remove(left); //combining fragments
			}
			right.addToTheLeft(left);
		}
		else { //exactly parallel, just reversed
			if (!fragments.contains(left)) {
				fragments.add(left);
			}
			else if (fragments.contains(right)) {
				fragments.remove(right);
			}
			left.addToTheRight(right);
		}	
	}
	public List<String> getFragments() {
		List<String> list = new ArrayList<String>();
		for (GarbleWord g : fragments) {
			list.add(g.getGarbleFragment());
		}
		return list;
	}
	
}
