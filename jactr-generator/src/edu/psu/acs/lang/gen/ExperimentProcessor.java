package edu.psu.acs.lang.gen;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import edu.psu.acs.lang.settings.ExperimentSettings;
import edu.psu.acs.lang.settings.ProcessConsts;
import edu.psu.acs.lang.util.NodeParser;


	//PROCESS STEPS:
	//1. Create a folder in the runs folder called expName
	//2. Copy the selected (or random) swbd text and ccg file that populates this run (can call them simply swbd.txt and swbd.ccg)
	//3. Create words.dsv and types.list from those, placing them in the folder
	//4. Create the model, placing it in the folder
	//5. Copy the model to the models folder, using the following format "lp-"+VERSION+"-"+expName+".jactr"
	
public class ExperimentProcessor {
	public static void main(String[] args) throws IOException {
		if (args.length == 1 && args[0].equals("-help")) {
			printHelp();
			System.exit(0);
		}
		String workingDir = null;
		String expVer = null;
		String runName = null;
		int fileNum = -1;
		for (int i = 0; i < args.length; i++) {
			if (args[i].charAt(0) == '-') {
				if (args.length <= i+1) {
					printHelp();
					System.exit(1);
				}
				if (args[i].equals("-dir")) {
					workingDir = args[i+1];
				}
				else if (args[i].equals("-ver")) {
					expVer = args[i+1];
				}
				else if (args[i].equals("-run")) {
					runName = args[i+1];
				}
				else if (args[i].equals("-f")) {
					fileNum = Integer.parseInt(args[i+1]);
				}
			}
			else {
				continue;
			}
		}
		workingDir = workingDir == null ? ExperimentSettings.workingDir : workingDir;
		expVer = expVer == null ? ExperimentSettings.expVersion : expVer;
		runName = runName == null ? ExperimentSettings.expName : runName;
		fileNum = fileNum == -1 ? ExperimentSettings.fileNum : fileNum;	
		Path runDir = Paths.get(ProcessConsts.getExpRunDir(workingDir, expVer, runName));
		Path newSent = Paths.get(ProcessConsts.getExpRunSent(workingDir, expVer, runName));
		Path newAnno = Paths.get(ProcessConsts.getExpRunAnno(workingDir, expVer, runName));
		Path wordInfo = Paths.get(ProcessConsts.getWordsInfoPath(workingDir, expVer, runName));
		Path typesList = Paths.get(ProcessConsts.getTypesListPath(workingDir, expVer, runName));
		String delimiter = ProcessConsts.delimiter;
		String modelName = ProcessConsts.getModelName(expVer, runName);
		Path writePath = Paths.get(ProcessConsts.getModelWritePath(workingDir, expVer, runName));
		Path runPath = Paths.get(ProcessConsts.getModelRunPath(workingDir, expVer, runName));
	
		
		int numDivisions = ProcessConsts.getNumDivisions(workingDir, expVer);

		Path sentChoice = ProcessConsts.getSentChoice(workingDir, expVer, fileNum, numDivisions).toPath();
		Path annoChoice = ProcessConsts.getAnnoChoice(workingDir, expVer, fileNum, numDivisions).toPath();
		
		runDir.toFile().mkdir();
	
		SWBDAggregator agg = createAuxFiles(sentChoice, annoChoice, newSent, newAnno, wordInfo, typesList, delimiter);
		ModelCreator mc = new ModelCreator(agg.getMaxWords(), agg.getMaxTypes());
		ModelWriter mw = new ModelWriter(writePath, modelName, mc);
		mw.writeAll(typesList, wordInfo, delimiter, newSent);
		Files.copy(writePath, runPath);
	}

	private static void printHelp() {
		System.out.println("This program has no necessary arguments, as it can be ran using the defaults found in ExperimentSettings.java");
		System.out.println("Possible arguments:");
		System.out.println("-dir    : specifies the directory the project is found in");
		System.out.println("-ver    : specifies the experiment version running");
		System.out.println("-run    : specifies the name of this specific experiment run");
		System.out.println("-f      : specifies the file number to be used by this run.");
	}
	private static SWBDAggregator createAuxFiles(Path sentChoice, Path annoChoice, Path newSent, Path newAnno, Path wordAux, Path typesAux, String delimiter) {
		SWBDAggregator agg = new SWBDAggregator();
		NodeParser p = agg.getNodeParser(annoChoice);	
		try {
			Files.copy(sentChoice, newSent);
			agg.recreateTypeFile(p, newAnno);
			agg.createWords(p, delimiter, wordAux, newSent);
			agg.createTypesList(wordAux, typesAux);
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return agg;
	}
}
