package edu.psu.acs.lang.output;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.RunConsts;
import edu.psu.acs.lang.hook.UtilityInitialization;
import edu.psu.acs.lang.output.eval.GarbleFragments;

public class OutputReader {
	public static void translateToText(UtilityInitialization ui) throws IOException {
		FileWriter fw = new FileWriter(RunConsts.getOutSentencesFilePath(ui).toFile());
		List<OutputSentence> all = OutputReader.getAllSentences(ui);
		for (OutputSentence os : all) {
			GarbleFragments gf = new GarbleFragments(os);
			List<String> fragments = gf.getFragments();
			String outline = "";
			for (String f : fragments) {
				outline += f + RunConsts.fragDelim;
			}
			if (outline.isEmpty()) {
				System.err.println("No Fragments found");
			}
			else {
				outline = outline.substring(0, outline.length()-RunConsts.fragDelim.length()) + System.getProperty("line.separator");
			}
			fw.write(outline);
			fw.flush();
		}
		fw.close();
	}
	public static List<OutputSentence> getAllSentences(UtilityInitialization ui) {
		List<OutputSentence> output = new ArrayList<OutputSentence>();
		File[] files = RunConsts.getAllOutputFiles(ui);
		try {
			for (File f : files) {
				output.addAll(readFile(f));
			}
		} catch (IOException e) {
			e.printStackTrace();
			System.err.println("A path is invalid");
			System.exit(1);
		}
		return output;
	}
	public static List<OutputSentence> readFile(File f) throws IOException {
		List<String> output = Files.readAllLines(f.toPath());
		List<OutputSentence> toReturn = new ArrayList<OutputSentence>();
		int index = 0; // first line is always sentence-end
		boolean bastardGoal = false;
		OutputSentence os = new OutputSentence();
		while (index < output.size()) {
			//hack but should work
			String outLine = output.get(index).substring(output.get(index).lastIndexOf(')')+2); 
			if (outLine.equals(RunConsts.sentenceDelimiter)) {
				//System.out.println("Starting new File");
				index++;
				continue;
			}
			if (RunConsts.goalLine(outLine)) {
				// great!
				bastardGoal = false;
				if (RunConsts.bastardGoal(outLine)) {
					bastardGoal = true;
				}
				index++;
				continue;
			}
			if (bastardGoal) {
				index++;
				continue; // continue until we get a non bastard goal
			}
			//gf.addGarble(outLine);
			os.add(outLine);
			index++;
			if (index == output.size()) {
				break;
			}
			String newOutLine = output.get(index).substring(output.get(index).lastIndexOf(')')+2);
			if (newOutLine.equals(RunConsts.sentenceDelimiter)) {			
				toReturn.add(os);
				os = new OutputSentence();
				index++;
			}
		}
		return toReturn;
	}
}
