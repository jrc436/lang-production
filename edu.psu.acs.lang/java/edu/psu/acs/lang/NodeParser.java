package edu.psu.acs.lang;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.CCGCompoundType;
import edu.psu.acs.lang.declarative.CCGType;
import edu.psu.acs.lang.production.SyntaxRuleType;

//a node is one of two things:
		//recursive case:
		//a node is then a resultant type, a combinatory rule, and two nodes
		//base case:
		//a node is a type and a word
		
		//so first, we need logic to identify which case it is. 
		//In the base case, there will always be a closing bracket before the next opening bracket.
		//easy enough.. check if the indexof closing bracket is smaller than the indexof open bracket
		//in the base case, we just need to split up the type and the word. Yup, that's it. Of course, keep in mind that in the dependency
		//representation, nodes are at the rule level, so a base node is only one part of it!
		//We will use LexNodes to hold this.
		
		//Otherwise, there will ALWAYS be:
		//1. a resulting type. This will be the first thing that occurs, separated by whitespace from comb
		//2. A combinatory rule. This will be the second thing that occurs, separated by white space from result (also stuck inparens) and a left bracket from the nodes
		//3. 1 or 2 more nodes. These will both be enclosed in bracket sets. These could be recursive. Ideally put them right back into the nodecreator
		//We will use RuleNodes to hold this.
		
		
		//We can then feed the system back in and inverse it so it makes more sense.
public class NodeParser {
	private List<ParseNode> topNodes;
	private final boolean alternateSeperators;
	public List<ParseNode> getTops() {
		return new ArrayList<ParseNode>(topNodes);
	}
	public String toString() {
		String retval = "";
		for (ParseNode pn : topNodes) {
			retval += pn.toString() + System.lineSeparator();
		}
		return retval;
	}
	public NodeParser(Path path, boolean originalFiles) {
		this.alternateSeperators = !originalFiles;
		topNodes = new ArrayList<ParseNode>();
		List<String> lines = null;
		try {
			lines = Files.readAllLines(path);
		} catch (IOException e) {
			e.printStackTrace();
			System.err.println("Error reading from: "+path.toString());
			System.err.println("Closing...");
			System.exit(1);
		}
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			if (line.charAt(0) == '#' && line.contains("Incremental")) {
				break;
			}
			else if (line.charAt(0) == '#') {
				continue;
			}
			if (line.contains("TOP")) {
				String top = "";
				while (!line.contains("#") && i < lines.size()-1) {
					top += line;
					i++;
					line = lines.get(i);
				}
				i--; //the last line was the one that failed, so we might want to double check it again.
				topNodes.add(getNode(top));
			}
		}
	}
	public NodeParser(String file, boolean originalFiles) {
		this(Paths.get(file), originalFiles);
	}
	private ParseNode getNode(String node) {
		node = node.trim();
		if (node.charAt(0) != '{' && node.charAt(node.length()-1) != '}') {
			System.err.println("Error, there is a bug that allows for formatting exceptions! All nodes must be enclosed with brackets!");
			System.err.println(node);
			System.exit(1);
		}
		String noBNode = node.substring(1, node.length()-1); //remove front and end brackets
		noBNode = noBNode.trim(); //whitepsace is fairly unpredictable...
		if (!noBNode.contains("{")) {
			//no brackets??? must be a base case.
			String[] things = noBNode.split("\\s+"); 
			if (things.length != 2) { //error checking
				for (String thing : things) {
					System.err.println(thing);
				}
				System.exit(1);
			}
			return new LexNode(things[1], CCGCompoundType.makeCCGType(things[0], alternateSeperators));
		}
		//okay, otherwise we expect a resulting type and a combinatory rule before our first set of brackets. Let's peel off those two parts.
		int firstLeftBracket = noBNode.indexOf('{');
		String leftPart = noBNode.substring(0, firstLeftBracket);
		leftPart = leftPart.trim();
		String[] parts = leftPart.split("\\s+");
		if (parts.length != 2) { //error checking
			for (String thing : parts) {
				System.err.println(thing);
			}
			System.exit(1);
		}
		CCGType resultType = CCGCompoundType.makeCCGType(parts[0], alternateSeperators);
		SyntaxRuleType rule = SyntaxRuleType.parseSyntaxRule(parts[1]);
		//this is one or two nodes... it shouldn't be more, but we can make it fairly easy for it to not matter.
		String rightPart = noBNode.substring(firstLeftBracket, noBNode.length());
		rightPart = rightPart.trim();
		if (rightPart.charAt(0) != '{') {
			System.err.println("Error, our trimmer for complex nodes produced something that doesn't start with a bracket!");
			System.err.println(rightPart);
			System.exit(1);
		}
		int i = 0;
		int counter = 0;
		int lastEnclosureIndex = -1; //see code, we're always adding to lastEnclosureIndex because we don't want the actual index, right?
		List<String> enclosures = new ArrayList<String>();
		while (i < rightPart.length()) {
			if (rightPart.charAt(i) == '{') {
				counter++;
			}
			else if (rightPart.charAt(i) == '}') {
				counter--;
			}
			if (counter == 0) {
				//oh look, we've found an enclosure! let's do something.
				String enclosure = rightPart.substring(lastEnclosureIndex+1, i+1).trim();
				if (!enclosure.isEmpty()) {
					enclosures.add(enclosure); //have to do i+1 to include the index with the right curler
					lastEnclosureIndex = i; //the index of the final right brace of this enclosure.
				}
			}
			i++;
		}
		if (enclosures.size() == 0) {
			System.err.println("No enclosures found. What?");
			System.err.println(rightPart);
			System.exit(1);
		}
		else if (enclosures.size() > 2) {
			System.err.println("An unusual amount of enclosures found, possible error in the code");
			for (String enclosure : enclosures) {
				System.err.println(enclosure+"\n");
			}
			System.exit(1);
		}
		else if (enclosures.size() == 1) {
			//we check in the rulenode constructor to make sure that it's actually typeraising.
			return new RuleNode(resultType, getNode(enclosures.get(0)), null, rule);
		}
		return new RuleNode(resultType, getNode(enclosures.get(0)), getNode(enclosures.get(1)), rule);
	}
	
}
