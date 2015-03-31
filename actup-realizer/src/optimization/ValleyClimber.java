package optimization;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;

import runconfig.ModelType;
import runconfig.Settings;
import evaluation.Evaluator;
import evaluation.LevenshteinDistance;
import evaluation.LevenshteinDistanceWord;
import evaluation.Rouge;
import ngrams.ACTRNgramModel;
import realize.Realization;
import realize.RealizeMain;


//a hillclimber... that likes valleys
public class ValleyClimber implements Optimizer {
	private String inputFileDir;
	private String realizationLogPath;
	private String goldRealizationDir;
	private String resultsLogPath;
	private Settings runSettings;
	private Evaluator eval;
	private String grammarFile;
	public ValleyClimber(Settings s, String grammarFile, String inputFileDir, String realizationLogPath, String resultsLogPath, String goldRealizationDir) {
		this.goldRealizationDir = goldRealizationDir;
		//this.opt = opt;
		this.inputFileDir = inputFileDir;
		this.realizationLogPath = realizationLogPath;
		this.resultsLogPath = resultsLogPath;
		this.runSettings = s;
		this.grammarFile = grammarFile;
		try { 
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
		catch (IOException io) {
			io.printStackTrace();
			System.err.println("Unable to initialize evaluator!");
			System.exit(1);
		}
	}
//	public void randomRestart() {
//		opt.forceRandomAll();
//	}
//	public void resetOpt(double[] vars) {
//		opt.forceAll(vars);
//	}
	//startIndex refers to which variable to optimize first!
	
	//experiment name should be a specific run of an experiment... which should be independent of the settings
	//and only dependent on opt
	public VariableSet optimizeVariables(String experimentName, VariableSet opt, int maxIter) {
		RealizeMain r = null;
		FileWriter fw = null;
		try {
			r = new RealizeMain(this.grammarFile);
			fw = new FileWriter(resultsLogPath+experimentName);
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
			iterName = experimentName+"-i"+String.format("%04d", currentIter);
			lastScore = currentScore;
			try {
				currentScore = performRun(iterName, opt, r, fw);
			}
			catch (IOException e) {
				e.printStackTrace();
				System.err.println("Error when logging!!");
			}
			//making strictly less will let it terminate a bit faster, but will explore less values
			boolean goodStep = currentScore <= lastScore;
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
	
	private double performRun(String runName, VariableSet opt, RealizeMain r, FileWriter fw) throws IOException {	
		//String num = in.getName().split("-")[1];
		System.out.println("This realization is a: " + runSettings.toString());
		File[] list = new File(this.inputFileDir).listFiles();
		Realization[] rOut = null;
		for (File in : list) {
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
			eval.loadFiles(goldRealizationDir, realizationLogPath);
		}
		else {
			eval.loadData(rOut);
		}
		double score = eval.scoreAll().getScore();
		
		fw.write(runName+":: "+"score: "+score+"; "+actrVarToString(opt)+"\n");
		fw.flush();
		return score;
	}
}
