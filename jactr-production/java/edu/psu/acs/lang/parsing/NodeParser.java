package edu.psu.acs.lang.parsing;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Stack;

import edu.psu.acs.lang.declarative.type.CCGBaseType;
import edu.psu.acs.lang.declarative.type.CCGBaseTypeEnum;
import edu.psu.acs.lang.declarative.type.CCGCompoundType;
import edu.psu.acs.lang.declarative.type.CCGType;
import edu.psu.acs.lang.production.SyntaxRuleType;
import util.collections.DoubleKeyMap;
import util.collections.Pair;

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
	private final List<ParseNode> topNodes;
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
	public NodeParser(List<String> ccgTerms, boolean originalFiles) throws ParseException {
		this.alternateSeperators = !originalFiles;
		this.topNodes = new ArrayList<ParseNode>();
		for (String s : ccgTerms) {
			List<String> lines = Arrays.asList(s.split(System.getProperty("line.separator")));
			this.topNodes.addAll(popTops(lines, "unknown"));
		}
	}
	public NodeParser(String file, boolean originalFiles) throws ParseException {
		this(Paths.get(file), originalFiles);
	}
	public NodeParser(NodeParser np) {
		this.topNodes = new ArrayList<ParseNode>(np.topNodes);
		this.alternateSeperators = np.alternateSeperators;
	}
	public NodeParser() {
		this.topNodes = new ArrayList<ParseNode>();
		this.alternateSeperators = true;
	}
	public NodeParser(Path path, boolean originalFiles) throws ParseException {
		this.alternateSeperators = !originalFiles;		
		List<String> lines = null;
		try {
			lines = Files.readAllLines(path);
		} catch (IOException e) {
			e.printStackTrace();
			System.err.println("Error reading from: "+path.toString());
			System.err.println("Closing...");
			System.exit(1);
		}
		topNodes = popTops(lines, path.toString());
	}
	private List<ParseNode> popTops(List<String> lines, String fileName) throws ParseException {
		System.out.println("Starting parse of: "+fileName);
		List<ParseNode> topNodes = new ArrayList<ParseNode>();
		//boolean skipNext = false;
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			if (line.charAt(0) == '#') { //these are comment lines, though it doesn't matter in the rewrite
				continue;
			}
			if (line.contains("TOP")) {
				if (line.indexOf("{") != 0) {
					System.err.println("TOP line does not start with an open bracket. The current idea is that it should.");
					throw new IllegalArgumentException();
				}
				int openbrackets = 0;
				String top = "";
				String cline = line;
				while (i < lines.size()) {
					for (int j = 0; j < cline.length(); j++) {
						if (cline.charAt(j) == '{') {
							openbrackets++;
						}
						else if (cline.charAt(j) == '}') {
							openbrackets--;
						}
						top += cline.charAt(j);
						if (openbrackets == 0) {
							break;
						}
					}
					if (openbrackets == 0) {
						break;
					}
					i++;
					cline = lines.get(i);
				}
				//i--; //this should no longer be necessary - we'll break as soon as we're done.
				ParseNode t = getNode(top, fileName, i);
				if (t != null) {
					//System.out.println("Top successfully added");
					topNodes.add(t);
				}		
			}
		}
		return topNodes;
	}
	public void addTop(ParseNode pn) {
		this.topNodes.add(pn);
	}
	public static boolean testPurity(String ccgTerms) {
		NodeParser p = null;
		try {
			List<String> s = new ArrayList<String>();
			s.add(ccgTerms);
			p = new NodeParser(s, true);
		}
		catch (ParseException pe) {
			System.err.println(pe.getMessage());
			return false;
		}
		return !p.checkForUnsupportedTypeRaise();
	}
	public boolean checkForUnsupportedTypeRaise() {
		for (ParseNode n : this.getTops()) {
			Stack<ParseNode> s = new Stack<ParseNode>();
			s.push(n);
			while (!s.empty()) {
				ParseNode cur = s.pop();
				if (checkUnsupportedTypeRaise(cur)) {
					return true;
				}
				if (cur instanceof RuleNode) {
					RuleNode rcur = (RuleNode) cur;
					s.push(rcur.getLeftChild());
					s.push(rcur.getRightChild());				
				}
			}
		}
		return false;
	}
	public static DoubleKeyMap<String, CCGType, Integer> wordTypes(String ccgTerms) {
		NodeParser p = null;
		try {
			List<String> s = new ArrayList<String>();
			s.add(ccgTerms);
			p = new NodeParser(s, true);
		}
		catch (ParseException pe) {
			System.err.println(pe.getMessage());
			return null;
		}
		return p.wordTypes();
	}
	public static DoubleKeyMap<String, CCGType, Integer> wordTypes(ParseNode n) {
		DoubleKeyMap<String, CCGType, Integer> wordtypes = new DoubleKeyMap<String, CCGType, Integer>();
		Stack<ParseNode> s = new Stack<ParseNode>();
		s.push(n);
		while (!s.empty()) {
			ParseNode cur = s.pop();
			String key = null;
			CCGType val = null;
			if (cur instanceof RuleNode) {
				RuleNode rcur = (RuleNode) cur;
				if (!checkUnsupportedTypeRaise(cur) && (rcur.getRule() == SyntaxRuleType.TCR || rcur.getRule() == SyntaxRuleType.TypeRaise || rcur.getRule() == SyntaxRuleType.TPC)) {
					key = rcur.getPhrase();
					val = rcur.getType();
				}
				s.push(rcur.getLeftChild());
				s.push(rcur.getRightChild());
			}
			else if (cur instanceof LexNode) {
				LexNode lcur = (LexNode) cur;
				key = lcur.getPhrase();
				val = lcur.getType();
			}
			if (key != null && !val.equals(CCGBaseType.makeType(CCGBaseTypeEnum.TOP, null))) {
				int addend = wordtypes.containsKey(key, val) ? wordtypes.get(key, val) : 0;
				wordtypes.put(key, val, addend + 1);
			}
		}
		return wordtypes;
	}
	public DoubleKeyMap<String, CCGType, Integer> wordTypes() {
		DoubleKeyMap<String, CCGType, Integer> wordtypes = new DoubleKeyMap<String, CCGType, Integer>();
		for (ParseNode n : this.getTops()) {
			DoubleKeyMap<String, CCGType, Integer> ind = wordTypes(n);
			combine(wordtypes, ind);
		}
		return wordtypes;
	}
	private static void combine(DoubleKeyMap<String, CCGType, Integer> first, DoubleKeyMap<String, CCGType, Integer> second) {
		for (Pair<String, CCGType> keys : second.keySet()) {
			int addend = first.containsKey(keys) ? first.get(keys) : 0;
			first.put(keys, addend + second.get(keys));
		}
	}
	private static boolean checkUnsupportedTypeRaise(ParseNode pn) {
		if (!(pn instanceof RuleNode)) {
			return false;
		}
		RuleNode rn = (RuleNode) pn;
		if (rn.getPhrase().equals("TOP")) {
			return false;
		}
		if (rn.getRule() == SyntaxRuleType.TypeRaise || rn.getRule() == SyntaxRuleType.TCR || rn.getRule() == SyntaxRuleType.TPC) {
			if (rn.getPhrase().contains(" ") && (rn.getLeftChild() == null || rn.getLeftChild() instanceof LexNode) && (rn.getRightChild() == null || rn.getRightChild() instanceof LexNode)) {
				return true;
			}
			if (rn.getLeftChild() instanceof RuleNode) {
				return doubleCheck(rn);
			}
		}
		return false;
	}
	private static boolean doubleCheck(RuleNode cur) {
		boolean allTypeRaise = true;
		while (cur != null) {
			allTypeRaise = allTypeRaise && (cur.getRule() == SyntaxRuleType.TypeRaise || cur.getRule() == SyntaxRuleType.TCR || cur.getRule() == SyntaxRuleType.TPC);
			if (cur.getLeftChild() instanceof RuleNode) {
				cur = (RuleNode) cur.getLeftChild();
			}
			else {
				cur = null;
			}
		}
		return allTypeRaise;
	}
	
	private ParseNode getNode(String node, String fileName, int lineNum) throws ParseException {
		node = node.trim();
		if (node.charAt(0) != '{' && node.charAt(node.length()-1) != '}') {
			System.err.println("Error, there is a bug that allows for formatting exceptions! All nodes must be enclosed with brackets!");
			System.err.println(node);
			System.err.println(fileName +":"+ lineNum);
			throw new ParseException();
		}
		String noBNode = node.substring(1, node.length()-1); //remove front and end brackets
		noBNode = noBNode.trim(); //whitepsace is fairly unpredictable...
		if (!noBNode.contains("{")) {
			//no brackets??? must be a base case.
			String[] things = noBNode.split("\\s+"); 
			if (things.length != 2) { //error checking
				System.err.println("Printing Node... it seems to have an odd number of elements");
				for (String thing : things) {					
					System.err.println(thing);
				}
				System.err.println(fileName +":"+ lineNum);
				throw new ParseException();
			}
			CCGType t = CCGCompoundType.makeCCGType(things[0], alternateSeperators);
			if (t == null) {
				return null;
			}
			return new LexNode(things[1], t);
		}
		//okay, otherwise we expect a resulting type and a combinatory rule before our first set of brackets. Let's peel off those two parts.
		int firstLeftBracket = noBNode.indexOf('{');
		String leftPart = noBNode.substring(0, firstLeftBracket);
		leftPart = leftPart.trim();
		String[] parts = leftPart.split("\\s+");
		if (parts.length != 2) { //error checking		
			if (parts.length == 1) {
				//System.err.println("Guessing it's from a malformed PCT.");
				String firstPart = parts[0];
				parts = new String[2];
				parts[0] = firstPart;
				parts[1] = "(comb:PCT)";
			}
			else {
				System.err.println("Printing Node... it seems to have an odd number of elements");
				for (String thing : parts) {
					System.err.println(thing);
				}
				System.err.println(fileName +":"+ lineNum);
				throw new ParseException();
			}
		}
		CCGType resultType = null;
		try {
			resultType = CCGCompoundType.makeCCGType(parts[0], alternateSeperators);
		}
		catch (ParseException pe) {
			System.err.println("Encountered an error while parsing types.");
			System.err.println(fileName + ":" + lineNum);
			pe.printStackTrace();
			System.exit(1);
		}
		if (resultType == null) {
			return null;
		}
		SyntaxRuleType rule = SyntaxRuleType.parseSyntaxRule(parts[1]);
		//this is one or two nodes... it shouldn't be more, but we can make it fairly easy for it to not matter.
		String rightPart = noBNode.substring(firstLeftBracket, noBNode.length());
		rightPart = rightPart.trim();
		if (rightPart.charAt(0) != '{') {
			System.err.println("Error, our trimmer for complex nodes produced something that doesn't start with a bracket!");
			System.err.println(rightPart);
			System.err.println(fileName +":"+ lineNum);
			throw new ParseException();
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
			System.err.println(fileName +":"+ lineNum);
			throw new ParseException();
		}
		else if (enclosures.size() > 2) {
			System.err.println("An unusual amount of enclosures found, possible error in the code");
			System.err.println("Enclosure number: "+enclosures.size());
			System.err.println(enclosures.get(0));
			System.err.println(enclosures.get(enclosures.size()-1));
//			for (String enclosure : enclosures) {
//				System.err.println(enclosure+"\n");
//			}
			System.err.println(fileName +":"+ lineNum);
			throw new ParseException();
		}
		else if (enclosures.size() == 1) {
			//we check in the rulenode constructor to make sure that it's actually typeraising.
			ParseNode n = getNode(enclosures.get(0), fileName, lineNum);
			return n == null ? null : new RuleNode(resultType, n, null, rule);
		}
		ParseNode n = getNode(enclosures.get(0), fileName, lineNum);
		ParseNode m = getNode(enclosures.get(1), fileName, lineNum);
		return n == null || m == null ? null : new RuleNode(resultType, n, m, rule);
	}
	
}
