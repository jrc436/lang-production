package edu.psu.acs.lang.model;
//package edu.psu.acs.lang.gen;
//
//import java.io.FileWriter;
//import java.io.IOException;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.HashSet;
//import java.util.List;
//import java.util.Map;
//import java.util.Map.Entry;
//import java.util.Set;
//import java.util.Stack;
//
//import edu.psu.acs.lang.declarative.CCGType;
//import edu.psu.acs.lang.production.SyntaxRuleType;
//import edu.psu.acs.lang.util.LexNode;
//import edu.psu.acs.lang.util.NodeParser;
//import edu.psu.acs.lang.util.ParseException;
//import edu.psu.acs.lang.util.ParseNode;
//import edu.psu.acs.lang.util.RuleNode;
//
//public class SWBDAggregator {
//	
//	//private static final Pattern ccgRegex = Pattern.compile("swbd[0-9]+\\.ccg");
//	//private static final Pattern txtRegex = Pattern.compile("swbd[0-9]+\\.txt");
//	
//	//private final Path expDir;
//	//private final int divisionToUse;
//	//private File wordCat;
//	//private File typeCat;
////	private final NodeParser p;
////	private final Path typesList;
////	private final Path wordsInfo;
//	private Map<CCGType, Set<CCGType>> equivalences;
//	private int maxTypes;
//	private int maxWords;
//	public SWBDAggregator() {
//		//this.divisionToUse = divisionToUse;
//		//this.expDir = expDir;
////		FileFilter ccgFilter = new FileFilter() { 
////			public boolean accept(File f) {
////				return f.getName().endsWith(".ccg");
////			}
////		};
////		FileFilter txtFilter = new FileFilter() {
////			public boolean accept(File f) {
////				return f.getName().endsWith(".txt");
////			}
////		};
////		catFiles(ccgFilter, txtFilter, swordCat, stypeCat);
////		this.sentFile = sentFile;
////		this.annoFile = annoFile;
////		this.typesList = typesList;
////		this.wordsInfo = wordsInfo;
////		p = new NodeParser(annoFile, true);
//		equivalences = new HashMap<CCGType, Set<CCGType>>();
////		recreateTypeFile();
//	}
//	public int getMaxTypes() {
//		return maxTypes;
//	}
//	public int getMaxWords() {
//		return maxWords;
//	}
////	private int catFiles(FileFilter ccgFilter, FileFilter txtFilter, String swordCat, String stypeCat) throws IOException {
////		List<String> lines = new ArrayList<String>();
////		List<String> tlines = new ArrayList<String>();
////		File[] files = expDir.toFile().listFiles(txtFilter);
////		Arrays.sort(files);
////		File[] tfiles = expDir.toFile().listFiles(ccgFilter);
////		Arrays.sort(tfiles);
////		if (files.length != tfiles.length) {
////			System.err.println("Files: "+files.length+"; tfiles: "+tfiles.length);
////			for (int j = 0; j < Math.max(files.length, tfiles.length); j++) {
////				if (j < files.length && j >= tfiles.length) {
////					System.err.println("Files"+j+": "+files[j]+"Tfiles"+j+": "+"None");
////					break;
////				}
////				else if (j < tfiles.length && j >= files.length) {
////					System.err.println("Files"+j+": "+"None"+"Tfiles"+j+": "+tfiles[j]);
////					break;
////				}
////				else if (files[j] != tfiles[j]) {
////					System.err.println("Files"+j+": "+files[j]+"Tfiles"+j+": "+tfiles[j]);
////					break;
////				}
////			}
////			System.exit(1);
////		}
////		lines.addAll(Files.readAllLines(files[divisionToUse].toPath()));
////		tlines.addAll(Files.readAllLines(tfiles[divisionToUse].toPath()));
////		
////		
////		wordCat = expDir.resolve(swordCat).toFile();
////		typeCat = expDir.resolve(stypeCat).toFile();
////		
////		FileWriter fwword = new FileWriter(wordCat);
////		for (String line : lines) {
////			fwword.write(line+System.getProperty("line.separator"));
////		}
////		fwword.close();
////		
////		FileWriter fwtype = new FileWriter(typeCat);
////		for (String line : tlines) {
////			fwtype.write(line+System.getProperty("line.separator"));
////		}
////		fwtype.close();
////		return lines.size();
////	}
//	public NodeParser getNodeParser(Path baseAnnoFile) {
//		try {
//			return new NodeParser(baseAnnoFile, true);
//		} catch (ParseException e) {
//			System.err.println("Error parsing");
//			e.printStackTrace();
//			System.exit(1);
//		}
//		return null;
//	}
//	public void recreateTypeFile(NodeParser p, Path runAnnoPath) throws IOException {
//		List<ParseNode> parse = p.getTops();
//		for (ParseNode n : parse) {
//			Stack<ParseNode> s = new Stack<ParseNode>();
//			s.push(n);
//			while (!s.empty()) {
//				ParseNode cur = s.pop();
//				if (cur instanceof RuleNode) {
//					RuleNode rcur = (RuleNode) cur;
//					rcur.setLeftChild(grokPCT(grokTypeRaise(rcur.getLeftChild())));
//					rcur.setRightChild(grokPCT(grokTypeRaise(rcur.getRightChild())));
//					s.push(rcur.getLeftChild());
//					s.push(rcur.getRightChild());				
//				}
//			}
//			FileWriter fwtype = new FileWriter(runAnnoPath.toFile());
//			fwtype.write(p.toString());
//			fwtype.close();
//		}
//		//I believe this should just be done now. Just have to now write it back out.
//	}
//	
//	
//	
//	private ParseNode grokTypeRaise(ParseNode pn) {
//		if (!(pn instanceof RuleNode)) {
//			return pn;
//		}
//		RuleNode rn = (RuleNode) pn;
//		if (rn.getRule() == SyntaxRuleType.TypeRaise || rn.getRule() == SyntaxRuleType.TCR || rn.getRule() == SyntaxRuleType.TPC) {
//			System.out.println("Grokking type raise");
//			if (!equivalences.containsKey(rn.getLeftChild().getType())) {
//				equivalences.put(rn.getLeftChild().getType(), new HashSet<CCGType>());
//			}
//			if (!equivalences.containsKey(rn.getType())) {
//				equivalences.put(rn.getType(), new HashSet<CCGType>());
//			}
//			equivalences.get(rn.getLeftChild().getType()).add(rn.getType());
//			equivalences.get(rn.getType()).add(rn.getLeftChild().getType());
//			if (rn.getLeftChild() instanceof RuleNode) {
//				//dam this sux.
//				System.err.println("Purity test failed");
//				return rn;
//			}
//			return new LexNode(rn.getPhrase(), rn.getType());
//		}
//		return rn;
//	}
//	private ParseNode grokPCT(ParseNode pn) {
//		if (!(pn instanceof RuleNode)) {
//			return pn;
//		}
//		RuleNode rn = (RuleNode) pn;
//		if (rn.getRule() == SyntaxRuleType.PCT || rn.getRule() == SyntaxRuleType.UNK) {
//			if (rn.getLeftChild().getType().isPCT()) {
//				return rn.getRightChild();
//			}
//			else if (rn.getRightChild().getType().isPCT()) {
//				return rn.getLeftChild();
//			}
//			else {
//				System.err.println("One child should be PCT god damnit!");
//				System.exit(1);
//			}
//		}
//		return rn;
//	}
//	public int createTypesList(Path wordsInfo, Path typesList) throws IOException {
//		List<String> lines = Files.readAllLines(wordsInfo);
//		FileWriter write = new FileWriter(typesList.toFile());
//		Set<String> types = new HashSet<String>();
//		int maxtypes = 0;
//		for (String line : lines) {
//			String[] separated = line.split(":-:");
//			if (separated.length-1 > maxtypes) {
//				maxtypes = separated.length-1;
//			}
//			for (int i = 1; i < separated.length; i++) {
//				types.add(separated[i]);
//			}
//		}
//		for (String type : types) {
//			write.write(type+"\n");
//		}
//		write.close();
//		this.maxTypes = maxtypes;
//		return maxtypes;
//	}
//	private int maxWordsPerSentence(Path sentFile) throws IOException {
//		List<String> lines = Files.readAllLines(sentFile);
//		
//		int toReturn = 0;
//		for (String line : lines) {
//			int words = line.split(" ").length;
//			if (words > toReturn) {
//				toReturn = words;
//			}
//		}
//		return toReturn;
//	}
//	
//	/**
//	 * Creates the list of words/lexsyns with their associated types, separated by a delimiter
//	 * @param ccgPaths the paths of the ".ccg" files that are passed to it.
//	 * @param delimiter what the ".dsv" file should be separated by
//	 * @param writePath where the ".dsv" file that this outputs should go
//	 * @return returns the maximum number of words in a sentence
//	 * @throws IOException if any of the files given in CCGPaths aren't valid
//	 */
//	public int createWords(NodeParser p, String delimiter, Path wordsInfo, Path sentFile) throws IOException {
//		//first, we'll just grab the word and make a list of all of their types so that we have a clear idea
//		Path writePath = wordsInfo;
//		FileWriter write = new FileWriter(writePath.toFile());
//		HashMap<String, Set<String>> map = new HashMap<String, Set<String>>();
//		
//		List<ParseNode> parse = p.getTops();
//		List<LexNode> collect = new ArrayList<LexNode>();
//		while (!parse.isEmpty()) {
//			ParseNode pn = parse.remove(0);
//			if (pn instanceof RuleNode) {
//				RuleNode rn = (RuleNode) pn;
//				parse.add(rn.getLeftChild());
//				parse.add(rn.getRightChild());
////				if (rn.getRightChild() == null) {
////					//look, it must be some kind of typeraising. We need to basically create our own thing, here.
////					parse.add(new LexNode(rn.getPhrase(), rn.getType()));
////				}
////				else {
////					parse.add(rn.getRightChild());
////				}
//			}
//			else if (pn instanceof LexNode) {
//				collect.add((LexNode)pn);
//			}
//		}
//		for (LexNode lex : collect) {
//			if (map.containsKey(lex.getPhrase())) {
//				map.get(lex.getPhrase()).add(lex.typeToString());
//			}
//			else {
//				Set<String> types = new HashSet<String>();
//				types.add(lex.typeToString());
//				map.put(lex.getPhrase().toLowerCase(), types);
//			}
//		}
//		for (Entry<String, Set<String>> pair : map.entrySet()) {
//			//code means that we've found some shit that's not real!
//			if (pair.getValue().contains("CODE")) {
//				//continue; this shouldn't be a problem anymore, but I want to double check
//				System.err.println("Parsing algorithm captures CODES");
//			}
//			if (pair.getValue().contains("TOP")) {
//				continue; //not sure if this is generally right... but we'll try it. 
//			}
////			if (pair.getValue().size() == 1 && (pair.getValue().contains(".") || pair.getValue().contains(",") || pair.getValue().contains(":"))) {
////				continue;
////			}
////			if (pair.getValue().contains(CCGBaseTypeEnum.N.toString()) && !pair.getValue().contains(CCGBaseTypeEnum.NP.toString())) {
////				pair.getValue().add(CCGBaseTypeEnum.NP.toString()); //any noun can act as a noun phrase... sort of...
////			}
//			addEquivalences(pair.getValue());
//			String toWrite = pair.getKey()+delimiter;
//			for (String type : pair.getValue()) {
//				toWrite += type+delimiter;
//			}
//			toWrite = toWrite.substring(0, toWrite.length()-delimiter.length()) + System.getProperty("line.separator");
//			write.write(toWrite);
//		}
//		write.close();
//		int maxwords = this.maxWordsPerSentence(sentFile);
//		this.maxWords = maxwords;
//		return maxwords;
//	}
//	private void addEquivalences(Set<String> types) {
//		for (CCGType s : equivalences.keySet()) {
//			if (types.contains(s.toString())) {
//				for (CCGType cg : equivalences.get(s)) {
//					types.add(cg.toString());
//				}
//			}
//		}
//	}
//	//TODO:
//	//Delete Type raising rules. We're capturing the types, then go back and recreate this shit without them?
//	//How, though? When you read a Type-Raising RuleNode, replace it with a LexNode with the resultant type.
//	//Delete PCT rules. We don't want to deal with that shit right now, and it's not that important
//	//How though? When you read a PCT rule, one of them should be a LexNode with a PCT type. First, add to the enum
//	//to identify whether something is PCT, second, when you read that rule node, replace it with the node that is not PCT
//	//Next, handle CONJ
//	//How though? First, there are two types of Conj rules. One involves the conj type. That is fairly straightforward. This turns
//	//the other type into that Type + CONJ. We'll need to use a flag in the type, which means another slot (boo). That slot will be true
//	//or false. The CONJ rule's result if it's with two of the same types is to set that flag to false. If it's with some type and the CONJ type
//	//it's to set that flag to true.
//}