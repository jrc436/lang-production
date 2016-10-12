package edu.psu.acs.lang.output;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.psu.acs.lang.declarative.sentence.Sentence;
import edu.psu.acs.lang.output.tree.GarbleTree;
import edu.psu.acs.lang.settings.ExperimentSettings;

public class OutputReader {
	 private static boolean goalLine(String line) {
         return line.contains(ExperimentSettings.newGoal);
	 }
	 private static boolean bastardGoal(String line) {
	         return line.contains("-");
	 }
	 private static int getSentNumFromLine(String line) {
		 String sentenceName = line.substring(line.lastIndexOf(Sentence.nameConst));
		 return bastardGoal(line) ? Sentence.extractNumber(sentenceName.split("-")[0]) : Sentence.extractNumber(sentenceName);
	 }

	public static List<String> treeSentences(List<OutputSentence> all) {
		List<String> lines = new ArrayList<String>();
		for (OutputSentence os : all) {
			GarbleTree gf = new GarbleTree(os);
			lines.add(gf.getDelimitedFragLine());
		}
		return lines;
	}
	public static List<String> getFragments(String garbSentence) {
		return Arrays.asList(garbSentence.split(ExperimentSettings.fragDelim));
	}
	public static List<OutputSentence> getAllSentences(File[] files) {
		List<OutputSentence> output = new ArrayList<OutputSentence>();
		try {
			for (File f : files) {
				output.addAll(readFile(f).values());
			}
		} catch (IOException e) {
			e.printStackTrace();
			System.err.println("A path is invalid");
			System.exit(1);
		}
		return output;
	}
	//hack but should work unless parens are in the output strings
	private static String getLine(List<String> output, int index) {
		return output.get(index).substring(output.get(index).lastIndexOf(')')+2); 
	}
	public static Map<Integer, OutputSentence> readFile(File f) throws IOException {
		List<String> output = Files.readAllLines(f.toPath());
		Map<Integer, OutputSentence> toReturn = new HashMap<Integer, OutputSentence>();
		int index = 1; // first line is always sentence-end
		boolean checkForRepeat = false;
		while (index < output.size()) {	
			String outLine = getLine(output, index);
			if (goalLine(outLine)) {
				int num = getSentNumFromLine(outLine);
				if (toReturn.containsKey(num)) {
					checkForRepeat = true;
				}
				else {
					toReturn.put(num, new OutputSentence());
				}
				index++;
				while (index < output.size()) {
					outLine = getLine(output, index);
					if (outLine.equals(ExperimentSettings.sentenceDelimiter)) {
						checkForRepeat = false; //this could happen in an empty one.
						index++;
						break;
					}
					if (checkForRepeat) {
						OutputSentence os = toReturn.get(num); 
						if (os.contains(outLine)) {
							System.err.println("Warning: repeat found.. skipping full realization...");
							//need to do a skiparoo...
							while (index < output.size() && !goalLine(getLine(output, index))) {
								index++;
							}
							break; //we found a repeat, i guess!
						}
					}
					checkForRepeat = false;
					toReturn.get(num).add(outLine);
					index++;
				}
			}
			else {
				System.err.println(index+":"+outLine);
				throw new IOException("This should always result in a goalLine at this point.");
			}
		}
		return toReturn;
	}
//	public static List<OutputSentence> readFile(File f) throws IOException {
//		List<String> output = Files.readAllLines(f.toPath());
//		List<OutputSentence> toReturn = new ArrayList<OutputSentence>();
//		int index = 0; // first line is always sentence-end
//		boolean bastardGoal = false;
//		
//		while (true) {
//			//hack but should work unless parens are in the output strings
//			String outLine = output.get(index).substring(output.get(index).lastIndexOf(')')+2); 
//			
//			if (goalLine(outLine)) {
//				OutputSentence os = new OutputSentence();
//				int num = 
//				while (!outLine.equals(ExperimentSettings.sentenceDelimiter)) {
//					outLine = output.get(index).substring(output.get(index).lastIndexOf(')')+2); 
//					
//				}
//			}
//			
//			if (outLine.equals(ExperimentSettings.sentenceDelimiter)) {
//				//System.out.println("Starting new File");
//				index++;
//				continue;
//			}
//			if (goalLine(outLine)) {
//				// great!
//				bastardGoal = false;
//				if (bastardGoal(outLine)) {
//					bastardGoal = true;
//				}
//				index++;
//				continue;
//			}
//			if (bastardGoal) {
//				index++;
//				continue; // continue until we get a non bastard goal
//			}
//			//gf.addGarble(outLine);
//			os.add(outLine);
//			index++;
//			if (index == output.size()) {
//				break;
//			}
//			String newOutLine = output.get(index).substring(output.get(index).lastIndexOf(')')+2);
//			if (newOutLine.equals(ExperimentSettings.sentenceDelimiter)) {			
//				toReturn.add(os);
//				os = new OutputSentence();
//				index++;
//			}
//		}
//		return toReturn;
//	}
}
