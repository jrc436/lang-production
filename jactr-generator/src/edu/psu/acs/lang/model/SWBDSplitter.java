package edu.psu.acs.lang.model;
//package edu.psu.acs.lang.gen;
//
//import java.io.File;
//import java.io.FileWriter;
//import java.io.IOException;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.util.ArrayList;
//import java.util.HashMap;
//import java.util.HashSet;
//import java.util.List;
//import java.util.Map;
//import java.util.PriorityQueue;
//import java.util.Set;
//
//import edu.psu.acs.lang.declarative.CCGType;
//import edu.psu.acs.lang.settings.PreprocessConsts;
//import edu.psu.acs.lang.util.Counter;
//import edu.psu.acs.lang.util.DoubleKeyMap;
//import edu.psu.acs.lang.util.NodeParser;
//
//public class SWBDSplitter {
//	private final Path swbdTextPath;
//	private final Path swbdCCGPath;
//	private final String swbdTextName;
//	private final String swbdAnnoName;
//	private final Path splitDir;
//	
//	//private final Map<String, Set<CCGType>> wordTypes;
//	public SWBDSplitter(Path dataDir, Path splitDir, String swbdText, String swbdnno) {		
//		this.swbdTextPath = dataDir.resolve(swbdText);
//		this.swbdCCGPath = dataDir.resolve(swbdnno);
//		this.swbdTextName = swbdText;
//		this.swbdAnnoName = swbdnno;
//		this.splitDir = splitDir;
//		//wordTypes = new HashMap<String, Set<CCGType>>();
//	}
//	private void preFilter(int maxLength, int maxTypes, List<String> sent, List<String> ccgTerms) {
//		DoubleKeyMap<String, CCGType, Integer> dkm = new DoubleKeyMap<String, CCGType, Integer>();
//		for (int i = 0; i < sent.size(); i++) {
//			if (sent.get(i).split(" ").length > maxLength || !NodeParser.testPurity(ccgTerms.get(i))) {
//				sent.remove(i);
//				ccgTerms.remove(i);
//				i--;
//				continue;
//			}
//			Map<String, Set<CCGType>> localTypes = NodeParser.wordTypes(ccgTerms.get(i));
//			for (String s : localTypes.keySet()) {
//				for (CCGType ccg : localTypes.get(s)) {
//					int val = dkm.containsKey(s, ccg) ? dkm.get(s, ccg) : 0;
//					val += 1;
//					dkm.put(s, ccg, val);
//				}
//			}
//		}
//		//now that we have the dkm, we have to assemble the maxTypes most populous types!
//		Map<String, PriorityQueue<Counter<CCGType>>> wordtypes = new HashMap<String, PriorityQueue<Counter<CCGType>>>();
//		for (String s : dkm.getKeysetOne()) {
//			wordtypes.put(s, new PriorityQueue<Counter<CCGType>>());
//			for (CCGType c : dkm.getFirstPairedKeys(s)) {
//				wordtypes.get(s).add(new Counter<CCGType>(c, dkm.get(s, c)));
//			}
//		}
//		Map<String, Set<CCGType>> useWordTypes = new HashMap<String, Set<CCGType>>();
//		for (String s : wordtypes.keySet()) {
//			int count = maxTypes;
//			useWordTypes.put(s, new HashSet<CCGType>());
//			PriorityQueue<Counter<CCGType>> pq = wordtypes.get(s);
//			while (count > 0 && !pq.isEmpty()) {
//				useWordTypes.get(s).add(pq.remove().getData());
//				count--;
//			}
//		}
//		for (int i = 0; i < sent.size(); i++) {
//			Map<String, Set<CCGType>> localTypes = NodeParser.wordTypes(ccgTerms.get(i));
//			boolean destroy =false;
//			for (String s : localTypes.keySet()) {
//				for (CCGType c : localTypes.get(s)) {
//					if (!useWordTypes.get(s).contains(c)) {
//						destroy = true;	
//						if (destroy) {
//							break;
//						}
//					}
//				}
//				if (destroy) {
//					break;
//				}
//			}
//			if (destroy) {
//				sent.remove(i);
//				ccgTerms.remove(i);
//				i--;
//			}
//		}
//	}
//
//	public int split(int desiredSentences, int maxSentenceLength, int maxTypes) throws IOException {
//		List<String> sentences = gatherSentences(Files.readAllLines(swbdTextPath));
//		List<String> ccgTerms = gatherCCG(Files.readAllLines(swbdCCGPath));
//		if (ccgTerms.size() != sentences.size()) {
//			System.err.println("Split is currently returning an incorrect number of sentences or CCG terms");
//			System.err.println("Sentences: "+sentences.size()+" CCG Terms: "+ccgTerms.size());
//		}
//		preFilter(maxSentenceLength, maxTypes, sentences, ccgTerms);
//		int numDivisions = sentences.size() / desiredSentences;
//
//		int divisionNumber = 0;
//		int sentenceCounter = 0;
//		FileWriter textWrite = new FileWriter(getSplitFilePath(this.swbdTextName, divisionNumber, numDivisions));
//		FileWriter ccgWrite = new FileWriter(getSplitFilePath(this.swbdAnnoName, divisionNumber, numDivisions));
//		for (int i = 0; i < sentences.size(); i++) {
//			textWrite.write(sentences.get(i));
//			ccgWrite.write(ccgTerms.get(i));
//			sentenceCounter++;
//			if (sentenceCounter == desiredSentences && divisionNumber < numDivisions) {
//				textWrite.close();
//				ccgWrite.close();
//				divisionNumber++;
//				sentenceCounter = 0;
//				textWrite = new FileWriter(getSplitFilePath(this.swbdTextName, divisionNumber, numDivisions));
//				ccgWrite = new FileWriter(getSplitFilePath(this.swbdAnnoName, divisionNumber, numDivisions));
//			}
//		}
//		textWrite.close();
//		ccgWrite.close();
//		return numDivisions;
//	}
//	private String readOneCCG(List<String> lines, int startingIndex) {
//		String line = lines.get(startingIndex);
//		String toRet = line+System.getProperty("line.separator");
//		int j = startingIndex+1;
//		line = lines.get(j);
//		while (!line.contains("NEW SENTENCE")) {
//			toRet += (line+System.getProperty("line.separator"));
//			j++;
//			if (j == lines.size()) {
//				break;
//			}
//			line = lines.get(j);
//		}
//		return toRet;
//	}
//	private List<String> gatherSentences(List<String> lines) {
//		List<String> allSentences = new ArrayList<String>();
//		for (int i = 0; i < lines.size(); i++) {
//			allSentences.add(destroyPunctuation(lines.get(i))+System.getProperty("line.separator"));
//		}
//		return allSentences;
//	}
//	private List<String> gatherCCG(List<String> lines) {
//		List<String> allCCG = new ArrayList<String>();
//		for (int i = 0; i < lines.size(); i++) {
//			if (lines.get(i).contains("NEW SENTENCE") && !lines.get(i+2).contains("CODE")) {
//				allCCG.add(readOneCCG(lines, i));
//			}	
//		}
//		return allCCG;
//	}
//	private File getSplitFilePath(String baseName, int divisionNumber, int numDivisions) {
//		return PreprocessConsts.getSplitFilePath(baseName, splitDir, divisionNumber, numDivisions);
//	}
//	private String destroyPunctuation(String str) {
//		return str.replace(" .", "").replace(" ?", "").replace(" :", "").replace(" ,", "").replace(". ", "").replace(", ", "").replace("? ", "");
//	}
//}
