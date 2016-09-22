package edu.psu.acs.lang.settings;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

public class PreprocessConsts {
	//VARIABLES
//	private static final String workingDir = ExperimentSettings.workingDir+"/";
//	private static final String expVersion = ExperimentSettings.expVersion+"/";
//	private static final String expName = ExperimentSettings.expName+"/";
	
	//CONSTANTS

	private static final String experimentsDirName = "experiments/";

	private static final String rawdataDirName = "rawdata/";
	private static final String runsDirName = "runs/";
	private static final String splitdataDirName = "splitdata/";


	public static final String swbdSentName = "swbd.txt";
	public static final String swbdAnnoName = "swbd.ccg";


	
	//FUNCTIONS

	public static String getProjDataDir(String workingDir) {
		return workingDir + rawdataDirName;
	}
	private static String getExperimentsDir(String workingDir) {
		return workingDir + experimentsDirName;
	}
	
	
	private static String getExpVerDir(String workingDir, String expVersion) {
		return getExperimentsDir(workingDir) + expVersion+"/";
	}
	public static String getRunsDir(String workingDir, String expVersion) {
		return getExpVerDir(workingDir, expVersion) + runsDirName;
	}
	public static String getExpRawdata(String workingDir, String expVersion) {
		return getExpVerDir(workingDir, expVersion) + rawdataDirName;
	}
	public static String getExpSplitData(String workingDir, String expVersion) {
		return getExpVerDir(workingDir, expVersion) + splitdataDirName;
	}

	
	public static File getSplitFilePath(String baseName, String workingDir, String expVersion, int divisionNumber, int numDivisions) {
		return getSplitFilePath(baseName, Paths.get(getExpSplitData(workingDir, expVersion)), divisionNumber, numDivisions);
	}
	public static File getSplitFilePath(String baseName, Path splitData, int divisionNumber, int numDivisions) {
		String formatter = "%0"+String.valueOf(numDivisions).length()+"d";
		String[] splitemup = baseName.split("\\.");
		String fname = splitemup[0];
		String ext = sum(1, ".", splitemup);
		return splitData.resolve(fname+String.format(formatter, divisionNumber)+"."+ext).toFile();
	}
	private static String sum(int start, String between, String...strings) {
		String ret = "";
		for (int i = start; i < strings.length; i++) {
			ret += strings[i] + between;
		}
		ret = ret.substring(0, ret.length()-between.length());
		return ret;
	}
	
	
	

	//Input
//	public static final File[] getFixedInputFiles() {
//		File dir = Paths.get(workingDir + dataDirName + expSentences).toFile();
//		return dir.listFiles();
//	}
//	public static final File getFixedInputFile(int num) {
//		return Paths.get(workingDir + dataDirName + expSentences).resolve("swbdcat"+String.format("%02d", num)+".input").toFile();
//	}
//	public static final File[] getInputFilesList() {
//		File dir = Paths.get(workingDir+dataDirName+expInput).toFile();
//		File[] allFolders = dir.listFiles();
//		File[] input = new File[allFolders.length];
//		for (int i = 0; i < allFolders.length; i++) {
//			input[i] = allFolders[i].toPath().resolve(wordCat).toFile();
//		}
//		return input;
//	}
	
//	//Output Paths
//	public static final File[] getAllOutputFiles(int expNum, UtilityInitialization ui) {
//		File outputDir = Paths.get(workingDir + dataDirName + outputFolder).toFile();
//		FileFilter filter = new FileFilter() { 
//			public boolean accept(File f) {
//				return f.getName().contains(ui.toString().toLowerCase()) && f.getName().contains(String.format("%02d", expNum)) && f.getName().endsWith("output");
//			}
//		};
//		return outputDir.listFiles(filter);
//	}
//	public static final File[] getAllOutputFiles(int expNum) {
//		File outputDir = Paths.get(workingDir + dataDirName + outputFolder).toFile();
//		FileFilter filter = new FileFilter() { 
//			public boolean accept(File f) {
//				return f.getName().contains(String.format("%02d", expNum)) && f.getName().endsWith("output");
//			}
//		};
//		return outputDir.listFiles(filter);
//	}
//	public static final File[] getAllOutputFiles(UtilityInitialization ui) {
//		File outputDir = Paths.get(workingDir + dataDirName + outputFolder).toFile();
//		FileFilter filter = new FileFilter() { 
//			public boolean accept(File f) {
//				return f.getName().contains(ui.toString().toLowerCase()) && f.getName().endsWith("output");
//			}
//		};
//		return outputDir.listFiles(filter);
//	}	
//	public static final Path getOutSentencesFilePath(UtilityInitialization ui) {
//		return Paths.get(workingDir + dataDirName + outputSentences + ui.toString().toLowerCase() + ".txt"); 
//	}
//	public static final Path getDistributionFilePath(UtilityInitialization ui) {
//		return Paths.get(workingDir + dataDirName + distFolder + ui.toString().toLowerCase() + ".dist"); 
//	}
//	public static final Path getTableFilePath(UtilityInitialization ui) {
//		return Paths.get(workingDir + dataDirName + distFolder + ui.toString().toLowerCase() + ".table"); 
//	}
//	private static final String getPathFromData(UtilityInitialization ui) {
//		return dataDirName + outputFolder + ExperimentSettings.expName + "-" + ui.toString().toLowerCase()+FilePathConsts.outputName;
//	}
	
//	public static final String getOutputPath(UtilityInitialization ui) {
//		return workingDir + getPathFromData(ui);
//	}
//
//	public static final String getOutputFromRuns(UtilityInitialization ui) {
//		return "../../../../"+getPathFromData(ui);
//	}
}
