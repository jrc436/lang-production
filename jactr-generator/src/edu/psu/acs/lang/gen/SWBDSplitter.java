package edu.psu.acs.lang.gen;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import edu.psu.acs.lang.PathConsts;

public class SWBDSplitter {
	private final Path swbdTextPath;
	private final Path swbdCCGPath;
	public SWBDSplitter(Path dataDir, String swbdBaseName) {		
		this.swbdTextPath = dataDir.resolve(swbdBaseName+PathConsts.sentExt);
		this.swbdCCGPath = dataDir.resolve(swbdBaseName+PathConsts.sentAnnoExt);
	}
	public int split(int divisions, Path expDir) throws IOException {
		expDir.toFile().mkdir();
		List<String> lines = Files.readAllLines(swbdTextPath);
		int numSentences = lines.size();
		if (divisions > numSentences) { 
			divisions = numSentences; //no point in having 0 sentence per file, now is there...
		}
		int sentencesPerFile = numSentences / divisions;
		int divisionNumber = 1; 
		int numDigits = String.valueOf(divisions).length();
		String formatter = "%0"+numDigits+"d";
		FileWriter fw = new FileWriter(expDir.resolve(PathConsts.swbdBaseName+String.format(formatter, divisionNumber)+".txt").toFile());
		int sentenceCounter = 0;
		for (int i = 0; i < numSentences; i++) {
			lines.set(i, lines.get(i).replace(" .", "").replace(" ?", "").replace(" :", "").replace(" ,", "").replace(". ", "").replace(", ", "").replace("? ", ""));
			fw.write(lines.get(i)+System.getProperty("line.separator"));
			//if divisionNumber == divisions, we have to just throw the rest of them into the last one... a more elegant
			//solution probably isn't that necessary
			sentenceCounter++;
			if (sentenceCounter == sentencesPerFile && divisionNumber < divisions) {
				fw.close();
				divisionNumber++;
				sentenceCounter = 0;
				fw = new FileWriter(expDir.resolve(PathConsts.swbdBaseName+String.format(formatter, divisionNumber)+".txt").toFile());
			}
		}
		fw.close();
		
		divisionNumber = 1;
		sentenceCounter = 0;
		lines = Files.readAllLines(swbdCCGPath);
		fw = new FileWriter(expDir.resolve("swbd"+String.format(formatter, divisionNumber)+".ccg").toFile());
		for (int i = 0; i < lines.size(); i++) {
			if (lines.get(i).contains("NEW SENTENCE") && !lines.get(i+2).contains("CODE")) {
				String line = lines.get(i);
				fw.write(line+System.getProperty("line.separator"));
				int j = i+1;
				line = lines.get(j);
				sentenceCounter++;
				while (!line.contains("NEW SENTENCE")) {
					fw.write(line+System.getProperty("line.separator"));
					j++;
					if (j == lines.size()) {
						break;
					}
					line = lines.get(j);
				}
			}
			if (sentenceCounter == sentencesPerFile && divisionNumber < divisions) {
				fw.close();
				divisionNumber++;
				sentenceCounter = 0;
				fw = new FileWriter(expDir.resolve("swbd"+String.format(formatter, divisionNumber)+".ccg").toFile());
			}
			
		}
		fw.close();
		return divisions;
	}
}
