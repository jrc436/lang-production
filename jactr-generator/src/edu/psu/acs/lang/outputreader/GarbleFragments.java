package edu.psu.acs.lang.outputreader;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.psu.acs.lang.production.SyntaxRule;

public class GarbleFragments {
	private Set<GarbleWord> fragmentRoots; //should only include root words.
	public GarbleFragments() {
		this.fragmentRoots = new HashSet<GarbleWord>();
	}
	//may have to be updated;
	public static String getGarbleFromOutputLine(String outputLine) {
		outputLine = outputLine.substring(outputLine.indexOf(')'));
		return outputLine.trim();
	}
	private void rehash() {
		Set<GarbleWord> newSet = new HashSet<GarbleWord>();
		for (GarbleWord gw : fragmentRoots) {
			newSet.add(gw);
		}
		fragmentRoots = newSet;
	}
	public void addGarble(String garble) {
		rehash();
		//more or less our process:
		//determine if this is a combination of root fragments: both left and right are root fragments
		//determine if this is a new root fragment: neither left not right are root fragments
		//else, add to the left or right as necessary
		
		//if left was just added, that means RIGHT could be a root fragment. If left is also a root fragment, that means
		//it's a root fragment combination. If RIGHT is not a root fragment, that means it's a new root fragment.
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
		String[] leftParts = garbles[0].split(SyntaxRule.wordSep);
		IDWord leftdat = new IDWord(leftParts[0].trim(), Integer.parseInt(leftParts[1].trim()));
		String[] rightParts = garbles[1].split(SyntaxRule.wordSep);
		IDWord rightdat = new IDWord(rightParts[0].trim(), Integer.parseInt(rightParts[1].trim()));
		if (swaparoo) {
			IDWord tmp = leftdat;
			leftdat = rightdat;
			rightdat = tmp;
		}
		//System.out.println(i+": right: "+right.toString()+"; left: "+left.toString()+"; roots: "+fragmentRoots.toString());
		//the main thing we need to do here is potentially reroot. Basically, we want to reroot if either left or right
		//is contained in one of the fragments, but is not a current root. In the "leftJustAdded" case 
		GarbleWord left = null;
		GarbleWord right = null;
		for (GarbleWord g : fragmentRoots) {	
			//both of them could reroot any of them, but it's impossible for both of them to reroot the same fragment...
			//in that case
		//	GarbleWord rerootLeft = fragmentRoots.get(i).getRoot(left);
		//	GarbleWord rerootRight = fragmentRoots.get(i).getRoot(right);
			if (left == null) {
				left = g.getFromData(leftdat);
			}
			if (right == null) {
				right = g.getFromData(rightdat);
			}
//			if (left.equals(rerootLeft)) {
//				left = rerootLeft;
//			}
//			if (right.equals(rerootRight)) {
//				right = rerootRight;
//			}
		}
		if (left == null) {
			left = new GarbleWord(leftdat);
		}
		if (right == null) {
			right = new GarbleWord(rightdat);
		}
		//System.out.println(i+": right: "+right.toString()+"; left: "+left.toString()+"; roots: "+fragmentRoots.toString());
		boolean rightRoot = fragmentRoots.contains(right);
		boolean leftRoot = fragmentRoots.contains(left);
		
		if (!rightRoot && !leftRoot) { //new
			fragmentRoots.add(left); //on the very last call to this, this will be redundant. THis will normally be
			//handled by rerooting, but won't then.
			//System.out.println(left.toString()+":"+left.hashCode());
		}
		else if (rightRoot && leftRoot) { //merge
			fragmentRoots.remove(right);
		}
		GarbleWord.merge(left, right);	
	}
	private void clean() {
		for (GarbleWord g : fragmentRoots) {
			for (GarbleWord f : fragmentRoots) {
				if (g == f) {
					continue;
				}
				else if (g.getFromData(f.getData()) != null) {
					System.err.println("Spring cleaning: "+f.toString()+" ;; "+g.toString());
				}
			}
		}
	}
	public List<String> getFragments() {
		List<String> list = new ArrayList<String>();
		clean();
		for (GarbleWord g : fragmentRoots) {
			list.add(g.getGarbleFragment());
		}
		return list;
	}
	
}
