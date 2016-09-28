package edu.psu.acs.lang.model;
//package edu.psu.acs.lang.gen;
//
//import java.io.File;
//import java.io.IOException;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.nio.file.Paths;
//import java.util.LinkedList;
//import java.util.Queue;
//
//import edu.psu.acs.lang.settings.ExperimentSettings;
//import edu.psu.acs.lang.settings.PreprocessConsts;
//
////PREPROCESS STEPS:
//	//1. If Experiment Version doesn't exist, give an error message, asking the user to ensure they typed the directory correctly. If they are sure,
//	//remind them they are supposed to create the directory manually in the experiments folder (print it), then run again
//	//2. Check if the rawdata folder exists, check if the runs folder exists, check if the split-data folder exists, if yes skip to process, else go to step 3
//	//3. Create the rawdata folder by copying dataDir to expVerDir
//	//4. Create the runs folder, it should start empty
//	//5. Create the split-data folder by running the normal pipeline on the rawdata folder
//	
//	
//public class ExperimentPreprocessor {
//	public static void main(String[] args) throws IOException {
//		if (args.length == 1 && args[0].equals("-help")) {
//			printHelp();
//			System.exit(0);
//		}
//		String workingDir = null;
//		String expVer = null;
//		int maxLength = -1;
//		int maxTypes = -1;
//		int numSentencesPerSplit = -1;
//		for (int i = 0; i < args.length; i++) {
//			if (args[i].charAt(0) == '-') {
//				if (args.length <= i+1) {
//					printHelp();
//					System.exit(1);
//				}
//				if (args[i].equals("-dir")) {
//					workingDir = args[i+1];
//				}
//				else if (args[i].equals("-ver")) {
//					expVer = args[i+1];
//				}
//				else if (args[i].equals("-l")) {
//					maxLength = Integer.parseInt(args[i+1]);
//				}
//				else if (args[i].equals("-t")) {
//					maxTypes = Integer.parseInt(args[i+1]);
//				}
//				else if (args[i].equals("-n")) {
//					numSentencesPerSplit = Integer.parseInt(args[i+1]);
//				}
//			}
//			else {
//				continue;
//			}
//		}
//		workingDir = workingDir == null ? ExperimentSettings.workingDir : workingDir;
//		expVer = expVer == null ? ExperimentSettings.expVersion : expVer;
//		maxLength = maxLength == -1 ? ExperimentSettings.maxLength : maxLength;
//		maxTypes = maxTypes == -1 ? ExperimentSettings.maxTypes : maxTypes;
//		numSentencesPerSplit = numSentencesPerSplit == -1 ? ExperimentSettings.numberSentencesDesired : numSentencesPerSplit;
//		
//		Path expRawDataDir = Paths.get(PreprocessConsts.getExpRawdata(workingDir, expVer));
//		Path baseRawDataDir = Paths.get(PreprocessConsts.getProjDataDir(workingDir));
//		String sentFile = PreprocessConsts.swbdSentName;
//		String annoFile = PreprocessConsts.swbdAnnoName;
//		Path runsDir = Paths.get(PreprocessConsts.getRunsDir(workingDir, expVer));
//		Path splitDataDir = Paths.get(PreprocessConsts.getExpSplitData(workingDir, expVer));	
//		
//		SWBDSplitter sw = split(expRawDataDir, splitDataDir, runsDir, baseRawDataDir, sentFile, annoFile);
//		int numDivisions = -1;  
//		System.out.println("Preprocessing necessary, args values:");
//		printArgs(maxLength, maxTypes, numSentencesPerSplit);
//		numDivisions = sw.split(numSentencesPerSplit, maxLength, maxTypes);
//		System.out.println("Preprocessing complete. There are: "+numDivisions);
//	}
//	private static void printArgs(int length, int t, int split) {
//		System.out.println("-l      : "+length);
//		System.out.println("-t      : "+t);
//		System.out.println("-n      : "+split);
//	}
//	private static void printHelp() {
//		System.out.println("This program has no necessary arguments, as it can be ran using the defaults found in ExperimentSettings.java");
//		System.out.println("Possible arguments:");
//		System.out.println("-dir    : specifies the directory the project is found in");
//		System.out.println("-ver    : specifies the experiment version running");
//		System.out.println("-l      : specifies the max length the filter should allow in preprocessing.");
//		System.out.println("-t      : specifies the maximum number of types the filter should allow in preprocessing.");
//		System.out.println("-n      : specifies the number of sentences per split");
//	}
//	private static SWBDSplitter split(Path expRawdataDir, Path splitdataDir, Path runsDir, Path baseRawDataDir, String sentFileName, String annoFileName) throws IOException {
//		Path parent = expRawdataDir.getParent();
//		if (!parent.toFile().isDirectory()) {
//			System.err.println("Path: "+parent.toString()+" is not a valid directory. Please create a new experiment version directory manually and run again");
//			System.exit(1);
//		}
//		if (!expRawdataDir.toFile().exists()) {
//			recCopyDir(baseRawDataDir, expRawdataDir);
//		}
//		if (!runsDir.toFile().exists()) {
//			runsDir.toFile().mkdir();
//		}
//		if (!splitdataDir.toFile().exists()) {
//			splitdataDir.toFile().mkdir();		
//			//have to run the splitta
//		}
//		else {
//			System.err.println("Fatal error: folder has already been initialized. Please clean up if an error has occurred, and consider a new version.");
//			System.exit(1);
//		}
//		return new SWBDSplitter(expRawdataDir, splitdataDir, sentFileName, annoFileName);
//	}
//	private static void recCopyDir(Path from, Path to) throws IOException {
//		Queue<File> folders = new LinkedList<File>();
//		File cur = from.toFile();
//		File curTo = to.toFile();
//		Files.copy(cur.toPath(), to);
//		while (true) {
//			File[] list = cur.listFiles();
//			for (File f : list) {
//				Files.copy(f.toPath(), curTo.toPath().resolve(f.getName()));
//				if (f.isDirectory()) {
//					folders.offer(f);
//				}
//			}
//			if (folders.isEmpty()) {
//				break;
//			}
//			cur = folders.poll();
//			File par = cur;
//			String resolution = "";
//			while (!par.equals(from)) {
//				resolution += par.getName()+"/";
//				par = par.getParentFile();
//			}
//			curTo = curTo.toPath().resolve(resolution).toFile();
//		} 
//	}
//}
