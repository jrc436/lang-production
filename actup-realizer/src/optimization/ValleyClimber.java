package optimization;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Random;

import ngrams.ACTRNgramModel;
import realize.Realization;
import realize.RealizeMain;
import runconfig.IOSettings;
import runconfig.ModelType;
import runconfig.RealizationSettings;
import evaluation.Evaluator;
import evaluation.LevenshteinDistance;
import evaluation.LevenshteinDistanceWord;
import evaluation.Rouge;


//a hillclimber... that likes valleys
public class ValleyClimber implements Optimizer {
	private File[] inputFiles;
	private String realizationLogPath;
	private String goldRealizationDir;
	private String resultsLogPath;
	private IOSettings runSettings;
	private RealizationSettings rs;
	private Evaluator eval;
	private String grammarFile;
	
	public ValleyClimber(IOSettings s, RealizationSettings rs, String grammarFile, String inputFileDir, String resultsLogPath) {
		this(s, rs, grammarFile, inputFileDir, resultsLogPath, null, 1.0);
	}
	public ValleyClimber(IOSettings s, RealizationSettings rs, String grammarFile, String inputFileDir, String resultsLogPath, String realizationLogPath, double percentFiles) {
		this.rs = rs;
		inputFiles = this.getInputFiles(inputFileDir, percentFiles);
		
		this.realizationLogPath = realizationLogPath;
		this.resultsLogPath = resultsLogPath;
		this.runSettings = s;
		this.grammarFile = grammarFile;
		switch (s.getEvaluationType()) {
			case EditDistanceChar:
				eval = new LevenshteinDistance(s.getScoringStrategy());
				break;
			case EditDistanceWord:
				eval = new LevenshteinDistanceWord(s.getScoringStrategy());
				break;
			case ROUGE:
				eval = new Rouge(s.getScoringStrategy(), s.getEvaluationType().extPath());
				break;
		}
	}
	private File[] getInputFiles(String dir, double percentToUse) {
		File[] allInput = new File(dir).listFiles();
		int numFilesToUse = Math.max(1, (int) Math.round((double)allInput.length * percentToUse));
		File[] out = new File[numFilesToUse];
		Random r = new Random();
		for (int i = 0; i < numFilesToUse; i++) {
			out[i] = allInput[r.nextInt(allInput.length)];
		}
		return out;
	}
	
	//experiment name should be a specific run of an experiment... which should be independent of the settings
	//and only dependent on opt
	public VariableSet optimizeVariables(String experimentName, VariableSet opt, int maxIter) {
		RealizeMain r = null;
		FileWriter fw = null;
		try {
			//it's for obvious reasons very important that no one holds the same experiment name in a concurrent setup!!!
			r = new RealizeMain(this.rs, this.grammarFile);
			fw = new FileWriter(resultsLogPath+experimentName, true);
			fw.write(this.runSettings.toString()+"\n");
		}
		catch (IOException io) {
			io.printStackTrace();
			System.err.println("Unable to initialize realizer!");
			System.exit(1);
		}
		double currentScore = 0.0;
		double lastScore = currentScore;
		int currentIter = 1; 
		String iterName;
		while (true) {
			if (currentIter > maxIter) {
				break;
			}
			System.out.println("Iteration "+currentIter+"/"+maxIter);
			iterName = experimentName+"-i"+String.format("%04d", currentIter);
			lastScore = currentScore;
			
			currentScore = performRun(iterName, opt, r, fw);

			//making strictly less will let it terminate a bit faster, but will explore less values
			boolean goodStep = currentScore < lastScore;
			if (goodStep) { opt.acknowledgeImprovement(); }
			boolean stillMoving = opt.step(goodStep); //this checks if the variable itself is still improving
			if (!opt.updateIndex(stillMoving)) {
				break;
			}
			currentIter++;
//			//if it returns true, we just proceed with the incremented variable!
		}
		System.out.println("Score: "+currentScore);
		System.out.println("Completeness: "+eval.getCompleteness());
		try {
			fw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return opt;
	}
	
	//this can get the 3 digit number from an input file that can be used for the lm and output file
	private String parseRunNum(Path fp) {
		String name = fp.getFileName().toString();
		String[] sections = name.split("-");
		for (String section : sections) {
			try {
				Integer.parseInt(section);
				return section;
			}
			catch (NumberFormatException nfe) {
				continue;
			}
		}
		return "";
	}
	private String actrVarToString(VariableSet opt) {
		String s = "";
		double[] d = opt.getDoubleArray();
		if (runSettings.getModelType() == ModelType.ACTR) {
			s += ("negD: " + d[ACTRNgramModel.negD_index]+"; ");
			s += ("exp_year: "+d[ACTRNgramModel.ey_index]+"; ");
			s += ("pc_speaking: "+d[ACTRNgramModel.psp_index]+";");
			s += ("k: "+d[ACTRNgramModel.k_index]+";");
		}
		return s;
		
	}
	
	private double performRun(String runName, VariableSet opt, RealizeMain r, FileWriter fw)  {	
		//String num = in.getName().split("-")[1];
		System.out.println("This realization is a: " + runSettings.toString());
		Realization[] rOut = null;
		for (File in : inputFiles) {
			String num = parseRunNum(in.toPath());
			String iterName = runName + "-f" + num;
			
			System.out.println("Beginning to realize: "+iterName);
			r.setLM(runSettings, opt, num);
			try {
				String realizationLogPath = this.realizationLogPath == null ? null : this.realizationLogPath+iterName+".spl";
				rOut = r.realize(in.getPath().toString(), realizationLogPath);
			}
			catch (Exception e) {
				e.printStackTrace();
				System.err.println("Error while realizing");
				System.exit(1);
			}
		}
		if (goldRealizationDir != null) {
			//this should only generally be used if storing things in RAM is impractical for some reason.
			//not really thread safe as is.
			try {
				eval.loadFiles(goldRealizationDir, realizationLogPath);
			}
			catch (IOException io) {
				io.printStackTrace();
				System.err.println("Error loading from gold direcotry");
				System.exit(1);
			}
		}
		else {
			eval.loadData(rOut);
		}
		double score = eval.scoreAll().getScore();
		try {
			writeout(fw, runName, score, opt);
		}
		catch (IOException io) {
			io.printStackTrace();
			System.err.println("Error writing output");
			System.exit(1);
		}
		
		return score;
	}
	//this shouldn't need to be synchronized because every name that's being written to should be separate... but be careful!!
	private void writeout(FileWriter fw, String runName, double score, VariableSet opt) throws IOException {
		fw.write(runName+":: "+"score: "+score+"; "+actrVarToString(opt)+"\n");
		fw.flush();
	}
}
