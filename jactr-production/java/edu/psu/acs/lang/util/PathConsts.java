package edu.psu.acs.lang.util;

public class PathConsts {
	public static final String expName = "experiment2";
	public static final String swbdBaseName = "swbd";
	public static final String dataDirName = "data";
	public static final String workingDir = "/work/research/lang-production/";
	public static final String projectName = "jactr-production";
	public static final String models = "models";
	
	public static final String sentExt = ".txt";
	public static final String sentAnnoExt = ".ccg";
	public static final String wordsFName = "words.dsv";
	public static final String typesFName = "types.list";
	public static final String wordCat = "swbd.cat";
	public static final String typeCat = "types.cat";
	
	public static final String lmPath = workingDir + dataDirName + "/bnc.lm";
	public static final String outputName = "garbles.output";
	public static final String outputPath = workingDir + dataDirName + "/"+expName+ "/"+outputName;
}
