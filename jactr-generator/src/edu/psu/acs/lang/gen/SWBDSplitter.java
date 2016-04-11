package edu.psu.acs.lang.gen;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.util.NodeParser;
import edu.psu.acs.lang.util.PathConsts;

public class SWBDSplitter {
	private final Path swbdTextPath;
	private final Path swbdCCGPath;
	public SWBDSplitter(Path dataDir, String swbdBaseName) {		
		this.swbdTextPath = dataDir.resolve(swbdBaseName+PathConsts.sentExt);
		this.swbdCCGPath = dataDir.resolve(swbdBaseName+PathConsts.sentAnnoExt);
	}
	private void preFilter(int maxLength, List<String> sent, List<String> ccgTerms) {
		for (int i = 0; i < sent.size(); i++) {
			if (sent.get(i).split(" ").length > maxLength || !NodeParser.testPurity(ccgTerms.get(i))) {
				sent.remove(i);
				ccgTerms.remove(i);
				i--;
			}
		}
	}
	public int split(int desiredSentences, Path expDir, int maxSentenceLength) throws IOException {
		expDir.toFile().mkdir();
		List<String> sentences = gatherSentences(Files.readAllLines(swbdTextPath));
		List<String> ccgTerms = gatherCCG(Files.readAllLines(swbdCCGPath));
		if (ccgTerms.size() != sentences.size()) {
			System.err.println("Split is currently returning an incorrect number of sentences or CCG terms");
			System.err.println("Sentences: "+sentences.size()+" CCG Terms: "+ccgTerms.size());
		}
		preFilter(maxSentenceLength, sentences, ccgTerms);
		int numDivisions = sentences.size() / desiredSentences;

		int divisionNumber = 0;
		int sentenceCounter = 0;
		FileWriter textWrite = new FileWriter(getTextFilePath(expDir, divisionNumber, numDivisions));
		FileWriter ccgWrite = new FileWriter(getCCGFilePath(expDir, divisionNumber, numDivisions));
		for (int i = 0; i < sentences.size(); i++) {
			textWrite.write(sentences.get(i));
			ccgWrite.write(ccgTerms.get(i));
			sentenceCounter++;
			if (sentenceCounter == desiredSentences && divisionNumber < numDivisions) {
				textWrite.close();
				ccgWrite.close();
				divisionNumber++;
				sentenceCounter = 0;
				textWrite = new FileWriter(getTextFilePath(expDir, divisionNumber, numDivisions));
				ccgWrite = new FileWriter(getCCGFilePath(expDir, divisionNumber, numDivisions));
			}
		}
		textWrite.close();
		ccgWrite.close();
		return numDivisions;
	}
	private String readOneCCG(List<String> lines, int startingIndex) {
		String line = lines.get(startingIndex);
		String toRet = line+System.getProperty("line.separator");
		int j = startingIndex+1;
		line = lines.get(j);
		while (!line.contains("NEW SENTENCE")) {
			toRet += (line+System.getProperty("line.separator"));
			j++;
			if (j == lines.size()) {
				break;
			}
			line = lines.get(j);
		}
		return toRet;
	}
	private List<String> gatherSentences(List<String> lines) {
		List<String> allSentences = new ArrayList<String>();
		for (int i = 0; i < lines.size(); i++) {
			allSentences.add(destroyPunctuation(lines.get(i))+System.getProperty("line.separator"));
		}
		return allSentences;
	}
	private List<String> gatherCCG(List<String> lines) {
		List<String> allCCG = new ArrayList<String>();
		for (int i = 0; i < lines.size(); i++) {
			if (lines.get(i).contains("NEW SENTENCE") && !lines.get(i+2).contains("CODE")) {
				allCCG.add(readOneCCG(lines, i));
			}	
		}
		return allCCG;
	}
	private File getTextFilePath(Path expDir, int divisionNumber, int numDivisions) {
		String formatter = "%0"+String.valueOf(numDivisions).length()+"d";
		return expDir.resolve(PathConsts.swbdBaseName+String.format(formatter, divisionNumber)+PathConsts.sentExt).toFile();
	}
	private File getCCGFilePath(Path expDir, int divisionNumber, int numDivisions) {
		String formatter = "%0"+String.valueOf(numDivisions).length()+"d";
		return expDir.resolve(PathConsts.swbdBaseName+String.format(formatter, divisionNumber)+PathConsts.sentAnnoExt).toFile();
	}
	private String destroyPunctuation(String str) {
		return str.replace(" .", "").replace(" ?", "").replace(" :", "").replace(" ,", "").replace(". ", "").replace(", ", "").replace("? ", "");
	}
}
