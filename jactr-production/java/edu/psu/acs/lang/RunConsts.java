package edu.psu.acs.lang;

import java.io.File;
import java.io.FileFilter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import edu.psu.acs.lang.hook.UtilityInitialization;

public class RunConsts {
	public static final int numberSentencesDesired = 20;
	public static final int maxLength = 15;
	public static final UtilityInitialization UI = UtilityInitialization.LATE;
	
	public static final String sentenceDelimiter = "SENTENCE-END";
	public static final String newGoal = "NEW GOAL";
	public static final String fragDelim = "<%%>";
	
	public static final String expName = "experiment49";
	public static final String swbdBaseName = "swbd";
	public static final String dataDirName = "data/";
	public static final String workingDir = "/work/research/lang-production/";
	public static final String projectName = "jactr-production";
	public static final String models = "models";
	
	public static final String sentExt = ".txt";
	public static final String sentAnnoExt = ".ccg";
	public static final String wordsFName = "words.dsv";
	public static final String typesFName = "types.list";
	public static final String wordCat = "swbd.cat";
	public static final String typeCat = "types.cat";
	
	public static final String outputSentences = "output-sentences/";
	public static final String lmPath = workingDir + dataDirName + "/bnc-all.lm";
	public static final String srilmNgram = workingDir + "/srilm/bin/i686-m64/ngram";
	public static final String outputName = ".output2";
	public static final String outputFolder = "experiment-output/";
	public static final String distFolder = "experiment-distribution/";
	public static final String expInput = "experiment-data/";
	public static final String getOutputPath(UtilityInitialization ui) {
		return workingDir + getPathFromData(ui);
	}
	public static final File[] getInputFilesList() {
		File dir = Paths.get(workingDir+dataDirName+expInput).toFile();
		File[] allFolders = dir.listFiles();
		File[] input = new File[allFolders.length];
		for (int i = 0; i < allFolders.length; i++) {
			input[i] = allFolders[i].toPath().resolve(wordCat).toFile();
		}
		return input;
	}
	public static boolean goalLine(String line) {
		return line.contains(newGoal);
	}
	public static boolean bastardGoal(String line) {
		return line.contains("-");
	}
	public static final String getOutputFromRuns(UtilityInitialization ui) {
		return "../../../../"+getPathFromData(ui);
	}
	public static void writeLines(Path p, List<?> lines) throws IOException {
		FileWriter fw = new FileWriter(p.toFile());
		for (int i = 0; i < lines.size(); i++) {
			fw.write(lines.get(i).toString()+System.getProperty("line.separator"));
		}
		fw.close();
	}
	public static final File[] getAllOutputFiles(UtilityInitialization ui) {
		File outputDir = Paths.get(workingDir + dataDirName + outputFolder).toFile();
		FileFilter filter = new FileFilter() { 
			public boolean accept(File f) {
				return f.getName().contains(ui.toString().toLowerCase()) && f.getName().endsWith("output");
			}
		};
		return outputDir.listFiles(filter);
	}
	public static final Path getOutSentencesFilePath(UtilityInitialization ui) {
		return Paths.get(workingDir + dataDirName + outputSentences + ui.toString().toLowerCase() + ".txt"); 
	}
	public static final Path getDistributionFilePath(String ui) {
		return Paths.get(workingDir + dataDirName + distFolder + ui.toLowerCase() + ".dist"); 
	}
	public static final Path getTableFilePath(String ui) {
		return Paths.get(workingDir + dataDirName + distFolder + ui.toLowerCase() + ".table"); 
	}
	private static final String getPathFromData(UtilityInitialization ui) {
		return dataDirName + outputFolder + expName + "-" + ui.toString().toLowerCase()+RunConsts.outputName;
	}
}
