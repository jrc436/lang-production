package edu.psu.acs.lang.settings;

import edu.psu.acs.lang.hook.UtilityInitialization;

public class ExperimentSettings {
//	public static final int numberSentencesDesired = 10000;
//	public static final int maxLength = 25;
//	public static final int maxTypes = 25;
//	public static final int fileNum = 0;
	public static final UtilityInitialization UI = UtilityInitialization.DEFAULT;
	public static final String expVersion = "v1.5";
	public static final String expName = "smaller-finsts";
	public static final String workingDir = "/work/research/lang-production/";
	
	public static final String sentenceDelimiter = "SENTENCE-END";
	public static final String newGoal = "NEW GOAL";
	public static final String fragDelim = "<%%>";
	
//	private static final String modelName = "model";
	private static final String projName = "jactr-production/";
	private static final String modelsDirName = "models/";
	private static final String modelBase = "lp-";
	private static final String modelExt = ".jactr";
	
	private static String getModelsDirectory(String workingDir) {
		return workingDir + projName + "/"+modelsDirName;
	}
	public static String getModelRunPath(String workingDir, String expVersion, String expName) {
		return getModelsDirectory(workingDir) + getModelName(expVersion, expName);
	}
	public static String getModelName(String expVersion, String expName) {
		return modelBase + expVersion + "-"+expName+modelExt; 
	}
	
}
