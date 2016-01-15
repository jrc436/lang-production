package edu.psu.acs.lang.gen;

import java.io.File;
import java.io.FileFilter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;

import edu.psu.acs.lang.LexNode;
import edu.psu.acs.lang.NodeParser;
import edu.psu.acs.lang.ParseNode;
import edu.psu.acs.lang.PathConsts;
import edu.psu.acs.lang.RuleNode;
import edu.psu.acs.lang.declarative.CCGBaseTypeEnum;
import edu.psu.acs.lang.production.SyntaxRuleType;

public class ModelPreparer {
	
	//private static final Pattern ccgRegex = Pattern.compile("swbd[0-9]+\\.ccg");
	//private static final Pattern txtRegex = Pattern.compile("swbd[0-9]+\\.txt");
	
	private final Path expDir;
	private final int[] divisionsToUse;
	private File wordCat;
	private File typeCat;
	private final NodeParser p;
	public ModelPreparer(Path expDir, int[] divisionsToUse) throws IOException {
		this.divisionsToUse = divisionsToUse;
		this.expDir = expDir;
		FileFilter ccgFilter = new FileFilter() { 
			public boolean accept(File f) {
				return f.getName().endsWith(".ccg");
			}
		};
		FileFilter txtFilter = new FileFilter() {
			public boolean accept(File f) {
				return f.getName().endsWith(".txt");
			}
		};
		catFiles(ccgFilter, txtFilter);
		p = new NodeParser(typeCat.toPath(),true);
		recreateTypeFile();
	}
	private void catFiles(FileFilter ccgFilter, FileFilter txtFilter) throws IOException {
		List<String> lines = new ArrayList<String>();
		List<String> tlines = new ArrayList<String>();
		File[] files = expDir.toFile().listFiles(txtFilter);
		Arrays.sort(files);
		for (int division : divisionsToUse) {
			lines.addAll(Files.readAllLines(files[division].toPath()));
		}
		File[] tfiles = expDir.toFile().listFiles(ccgFilter);
		Arrays.sort(tfiles);
		for (int division : divisionsToUse) {
			tlines.addAll(Files.readAllLines(tfiles[division].toPath()));
		}
		
		wordCat = expDir.resolve(PathConsts.wordCat).toFile();
		typeCat = expDir.resolve(PathConsts.typeCat).toFile();
		
		FileWriter fwword = new FileWriter(wordCat);
		for (String line : lines) {
			fwword.write(line+System.getProperty("line.separator"));
		}
		fwword.close();
		
		FileWriter fwtype = new FileWriter(typeCat);
		for (String line : tlines) {
			fwtype.write(line+System.getProperty("line.separator"));
		}
		fwtype.close();
	}
	private void recreateTypeFile() throws IOException {
		List<ParseNode> parse = p.getTops();
		for (ParseNode n : parse) {
			Stack<ParseNode> s = new Stack<ParseNode>();
			s.push(n);
			while (!s.empty()) {
				ParseNode cur = s.pop();
				if (cur instanceof RuleNode) {
					RuleNode rcur = (RuleNode) cur;
					if (rcur.getLeftChild() instanceof RuleNode) {
						RuleNode lc = (RuleNode) rcur.getLeftChild();
						rcur.setLeftChild(grokTypeRaise(lc));
						rcur.setLeftChild(grokPCT(lc));
					}
					if (rcur.getRightChild() instanceof RuleNode) {
						RuleNode rc = (RuleNode) rcur.getRightChild();
						rcur.setRightChild(grokTypeRaise(rc));
						rcur.setRightChild(grokPCT(rc));
					}
					s.push(rcur.getLeftChild());
					s.push(rcur.getRightChild());				
				}
			}
			FileWriter fwtype = new FileWriter(typeCat);
			fwtype.write(p.toString());
			fwtype.close();
		}
		//I believe this should just be done now. Just have to now write it back out.
	}
	//returns either this, or 
	private ParseNode grokTypeRaise(RuleNode rn) {
		if (rn.getRule() == SyntaxRuleType.TypeRaise || rn.getRule() == SyntaxRuleType.TCR) {
			System.out.println("Grokking type raise");
			return new LexNode(rn.getPhrase(), rn.getType());
		}
		return rn;
	}
	private ParseNode grokPCT(RuleNode rn) {
		if (rn.getRule() == SyntaxRuleType.PCT) {
			if (rn.getLeftChild().getType().isPCT()) {
				return rn.getRightChild();
			}
			else if (rn.getRightChild().getType().isPCT()) {
				return rn.getLeftChild();
			}
			else {
				System.err.println("One child should be PCT god damnit!");
				System.exit(1);
			}
		}
		return rn;
	}
	public int createTypes() throws IOException {
		List<String> lines = Files.readAllLines(this.expDir.resolve(PathConsts.wordsFName));
		FileWriter write = new FileWriter(this.expDir.resolve(PathConsts.typesFName).toFile());
		Set<String> types = new HashSet<String>();
		int maxtypes = 0;
		for (String line : lines) {
			String[] separated = line.split(":-:");
			if (separated.length-1 > maxtypes) {
				maxtypes = separated.length-1;
			}
			for (int i = 1; i < separated.length; i++) {
				types.add(separated[i]);
			}
		}
		for (String type : types) {
			write.write(type+"\n");
		}
		write.close();	
		return maxtypes;
	}
	public int maxWordsPerSentence() throws IOException {
		List<String> lines = Files.readAllLines(wordCat.toPath());
		
		int toReturn = 0;
		for (String line : lines) {
			int words = line.split(" ").length;
			if (words > toReturn) {
				toReturn = words;
			}
		}
		return toReturn;
	}
	
	/**
	 * Creates the list of words/lexsyns with their associated types, separated by a delimiter
	 * @param ccgPaths the paths of the ".ccg" files that are passed to it.
	 * @param delimiter what the ".dsv" file should be separated by
	 * @param writePath where the ".dsv" file that this outputs should go
	 * @return returns the maximum number of words in a sentence
	 * @throws IOException if any of the files given in CCGPaths aren't valid
	 */
	public void createWords(String delimiter) throws IOException {
		//first, we'll just grab the word and make a list of all of their types so that we have a clear idea
		Path writePath = this.expDir.resolve(PathConsts.wordsFName);
		FileWriter write = new FileWriter(writePath.toFile());
		HashMap<String, Set<String>> map = new HashMap<String, Set<String>>();
		
		List<ParseNode> parse = p.getTops();
		List<LexNode> collect = new ArrayList<LexNode>();
		while (!parse.isEmpty()) {
			ParseNode pn = parse.remove(0);
			if (pn instanceof RuleNode) {
				RuleNode rn = (RuleNode) pn;
				parse.add(rn.getLeftChild());
				if (rn.getRightChild() == null) {
					//look, it must be some kind of typeraising. We need to basically create our own thing, here.
					parse.add(new LexNode(rn.getPhrase(), rn.getType()));
				}
				else {
					parse.add(rn.getRightChild());
				}
			}
			else if (pn instanceof LexNode) {
				collect.add((LexNode)pn);
			}
		}
		for (LexNode lex : collect) {
			if (map.containsKey(lex.getPhrase())) {
				map.get(lex.getPhrase()).add(lex.typeToString());
			}
			else {
				Set<String> types = new HashSet<String>();
				types.add(lex.typeToString());
				map.put(lex.getPhrase(), types);
			}
		}
		for (Entry<String, Set<String>> pair : map.entrySet()) {
			//code means that we've found some shit that's not real!
			if (pair.getValue().contains("CODE")) {
				//continue; this shouldn't be a problem anymore, but I want to double check
				System.err.println("Parsing algorithm captures CODES");
			}
//			if (pair.getValue().size() == 1 && (pair.getValue().contains(".") || pair.getValue().contains(",") || pair.getValue().contains(":"))) {
//				continue;
//			}
			if (pair.getValue().contains(CCGBaseTypeEnum.N.toString()) && !pair.getValue().contains(CCGBaseTypeEnum.NP.toString())) {
				pair.getValue().add(CCGBaseTypeEnum.NP.toString()); //any noun can act as a noun phrase... sort of...
			}
			String toWrite = pair.getKey()+delimiter;
			for (String type : pair.getValue()) {
				toWrite += type+delimiter;
			}
			toWrite = toWrite.substring(0, toWrite.length()-delimiter.length()) + System.getProperty("line.separator");
			write.write(toWrite);
		}
		write.close();
	}
	//TODO:
	//Delete Type raising rules. We're capturing the types, then go back and recreate this shit without them?
	//How, though? When you read a Type-Raising RuleNode, replace it with a LexNode with the resultant type.
	//Delete PCT rules. We don't want to deal with that shit right now, and it's not that important
	//How though? When you read a PCT rule, one of them should be a LexNode with a PCT type. First, add to the enum
	//to identify whether something is PCT, second, when you read that rule node, replace it with the node that is not PCT
	//Next, handle CONJ
	//How though? First, there are two types of Conj rules. One involves the conj type. That is fairly straightforward. This turns
	//the other type into that Type + CONJ. We'll need to use a flag in the type, which means another slot (boo). That slot will be true
	//or false. The CONJ rule's result if it's with two of the same types is to set that flag to false. If it's with some type and the CONJ type
	//it's to set that flag to true.
}
//for (String line : lines) {
//int start = line.indexOf('{');
//int end = line.indexOf('}');
//if (start == -1 || end == -1) {
//	continue;
//}
//String fragment = line.substring(start, end);
//String[] wordParts = fragment.split(" ");
//
////we have some kind of embedded bullshit. Most likely just have to move the starter pointer up
//while (wordParts.length > 4) {
//	fragment = fragment.substring(1);
//	int newStart = fragment.indexOf('{');
//	fragment = fragment.substring(newStart);
//	wordParts = fragment.split(" ");
//}
//List<String> parts = new ArrayList<String>(Arrays.asList(wordParts));
//parts.remove("");
//if (parts.contains(" ")) {
//	parts.remove(" ");
//	
//}
//
////ok, now we most likely either have a type raise operation or we have a just a word with its type.
//
////a "just in case" measure, if I forgot something
//if (parts.size() == 1) {
//	System.err.println("No spaces in fragment, printing and exiting:");
//	System.err.println(wordParts[0]);
//	System.exit(1);
//}
//else if (parts.size() == 3 || (parts.size() == 4 && !parts.get(1).contains("T"))) {
//	parts = new ArrayList<String>(Arrays.asList(new String[] { parts.get(1), parts.get(2) }));
//}
//
//String secondType = "";
//String type = "";
//String word = "";
//try {
//	secondType = parts.size() == 4 ? parts.get(2).substring(1) : "";
//	type = parts.get(0).substring(1);
//	int wordIndex = parts.size() == 4 ? 3 : 1;
//	word = parts.get(wordIndex);
//}
//catch (Exception e) {
//	e.printStackTrace();
//	System.err.println(parts.size());
//	for (String part : parts) {
//		System.err.println(part);
//	}
//	System.err.println(fragment);
//	System.exit(1);
//}
//word = word.toLowerCase();
//
////create or update the typeset
//Set<String> typeset;
//if (map.containsKey(word)) {
//	typeset = map.get(word);
//}
//else {
//	typeset = new HashSet<String>();
//	map.put(word, typeset);
//}
//typeset.add(type);
//if (secondType != "") {
//	typeset.add(secondType);
//}
//}