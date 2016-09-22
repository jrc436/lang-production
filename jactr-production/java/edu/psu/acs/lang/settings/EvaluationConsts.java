package edu.psu.acs.lang.settings;

import java.io.File;
import java.io.FileFilter;
import java.nio.file.Paths;

import edu.psu.acs.lang.hook.UtilityInitialization;

public class EvaluationConsts {
	public static final String sentenceDelimiter = "SENTENCE-END";
	public static final String newGoal = "NEW GOAL";
	public static final String fragDelim = "<%%>";
	public static boolean goalLine(String line) {
		return line.contains(newGoal);
	}
	public static boolean bastardGoal(String line) {
		return line.contains("-");
	}
//	//Output Paths
	public static final File[] getAllOutputFiles(int expNum, UtilityInitialization ui) {
		File outputDir = Paths.get(ProcessConsts.getExpRunDir(ExperimentSettings.workingDir, ExperimentSettings.expVersion, ExperimentSettings.expName)).toFile();
		FileFilter filter = new FileFilter() { 
			public boolean accept(File f) {
				return f.getName().contains(ui.toString().toLowerCase()) && f.getName().contains(String.format("%02d", expNum)) && f.getName().endsWith(ModelRunConsts.outputFileBase);
			}
		};
		return outputDir.listFiles(filter);
	}
	public static final File[] getAllOutputFiles(UtilityInitialization ui) {
		File outputDir = Paths.get(ProcessConsts.getExpRunDir(ExperimentSettings.workingDir, ExperimentSettings.expVersion, ExperimentSettings.expName)).toFile();
		FileFilter filter = new FileFilter() { 
			public boolean accept(File f) {
				return f.getName().contains(ui.toString().toLowerCase()) && f.getName().endsWith(ModelRunConsts.outputFileBase);
			}
		};
		return outputDir.listFiles(filter);
	}
	public static final File[] getAllOutputFiles(int expNum) {
		File outputDir = Paths.get(ProcessConsts.getExpRunDir(ExperimentSettings.workingDir, ExperimentSettings.expVersion, ExperimentSettings.expName)).toFile();
		FileFilter filter = new FileFilter() { 
			public boolean accept(File f) {
				return f.getName().contains(String.format("%02d", expNum)) && f.getName().endsWith(ModelRunConsts.outputFileBase);
			}
		};
		return outputDir.listFiles(filter);
	}
	public static final File[] getGoldFilesList() {
		File outputDir = Paths.get(ProcessConsts.getExpRunDir(ExperimentSettings.workingDir, ExperimentSettings.expVersion, ExperimentSettings.expName)).toFile();
		FileFilter filter = new FileFilter() { 
			public boolean accept(File f) {
				return f.getName().endsWith(PreprocessConsts.swbdSentName);
			}
		};
		return outputDir.listFiles(filter);
	}
	public static final File getFixedGoldFile(int i) {
		File outputDir = Paths.get(ProcessConsts.getExpRunDir(ExperimentSettings.workingDir, ExperimentSettings.expVersion, ExperimentSettings.expName)).toFile();
		FileFilter filter = new FileFilter() { 
			public boolean accept(File f) {
				return f.getName().contains(String.format("%02d", i)) && f.getName().endsWith(PreprocessConsts.swbdSentName);
			}
		};
		return outputDir.listFiles(filter)[0];
	}

	//Evaluation Consts
	public static final String rougePath = ExperimentSettings.workingDir+"rouge/rouge.pl";
	public static final String lmPath = PreprocessConsts.getExpRawdata(ExperimentSettings.workingDir, ExperimentSettings.expVersion)+ "/bnc-all.lm";
	public static final String srilmNgram = ExperimentSettings.workingDir + "/srilm/bin/i686-m64/ngram";
	
}
