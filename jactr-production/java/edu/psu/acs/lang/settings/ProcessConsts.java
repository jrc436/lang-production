package edu.psu.acs.lang.settings;

import java.io.File;
import java.io.FileFilter;
import java.nio.file.Path;
import java.nio.file.Paths;

public class ProcessConsts {
	
	private static final String modelName = "model";
	private static final String projName = "jactr-production/";
	private static final String modelsDirName = "models/";
	private static final String modelBase = "lp-";
	private static final String modelExt = ".jactr";
	private static final String wordsInfoName = "words.dsv";
	private static final String typesListName = "types.list";
	public static final String delimiter = ":-:";
	
	public static int getNumDivisions(String workingDir, String expVer) {
		Path split = Paths.get(PreprocessConsts.getExpSplitData(workingDir, expVer));
		return split.toFile().listFiles(txtFilter).length;
	}
	private static String getModelsDirectory(String workingDir) {
		return workingDir + projName + "/"+modelsDirName;
	}
	public static String getModelRunPath(String workingDir, String expVersion, String expName) {
		return getModelsDirectory(workingDir) + getModelName(expVersion, expName);
	}
	public static String getModelName(String expVersion, String expName) {
		return modelBase + expVersion + "-"+expName+modelExt; 
	}
	
	public static File getSentChoice(String workingDir, String expVersion, int divisionNumber, int numDivisions) {
		return PreprocessConsts.getSplitFilePath(PreprocessConsts.swbdSentName, workingDir, expVersion, divisionNumber, numDivisions);
	}
	public static File getAnnoChoice(String workingDir, String expVersion, int divisionNumber, int numDivisions) {
		return PreprocessConsts.getSplitFilePath(PreprocessConsts.swbdAnnoName, workingDir, expVersion, divisionNumber, numDivisions);
	}
	public static String getExpRunDir(String workingDir, String expVersion, String expName) {
		return PreprocessConsts.getRunsDir(workingDir, expVersion) + expName+"/";
	}
	public static String getExpRunSent(String workingDir, String expVersion, String expName) {
		return getExpRunDir(workingDir, expVersion, expName)+PreprocessConsts.swbdSentName;
	}
	public static String getExpRunAnno(String workingDir, String expVersion, String expName) {
		return getExpRunDir(workingDir, expVersion, expName)+PreprocessConsts.swbdAnnoName;
	}
	public static String getWordsInfoPath(String workingDir, String expVersion, String expName) {
		return getExpRunDir(workingDir, expVersion, expName) + wordsInfoName;
	}
	public static String getTypesListPath(String workingDir, String expVersion, String expName) {
		return getExpRunDir(workingDir, expVersion, expName) + typesListName;
	}

	public static String getModelWritePath(String workingDir, String expVersion, String expName) {
		return getExpRunDir(workingDir, expVersion, expName) + modelName + modelExt;
	}
	
	
	private static FileFilter txtFilter = new FileFilter() {
		public boolean accept(File f) {
			return f.getName().endsWith(".txt");
		}
	};
}
