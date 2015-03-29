package opennlp.ccg.optimization;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;

import opennlp.ccg.ModelType;
import opennlp.ccg.Settings;
import opennlp.ccg.evaluation.Evaluator;
import opennlp.ccg.evaluation.LevenshteinDistanceWord;
import opennlp.ccg.evaluation.LevenshteinDistance;
import opennlp.ccg.evaluation.Rouge;
import opennlp.ccg.realize.Realization;
import opennlp.ccg.realize.RealizeMain;


//a hillclimber... that likes valleys
public class ValleyClimber implements Optimizer {
	private RealizeMain r;
	private Variable[] opt;
	private String inputFileDir;
	private String realizationLogPath;
	private String goldRealizationDir;
	private String resultsLogPath;
	private Settings runSettings;
	private Evaluator eval;
	public ValleyClimber(Settings s, String grammarFile, Variable[] opt, String inputFileDir, String realizationLogPath, String resultsLogPath, String goldRealizationDir) {
		try {
			this.r = new RealizeMain(grammarFile);
		}
		catch (IOException io) {
			io.printStackTrace();
			System.err.println("Unable to initialize realizer!");
			System.exit(1);
		}
		this.goldRealizationDir = goldRealizationDir;
		this.opt = opt;
		this.inputFileDir = inputFileDir;
		this.realizationLogPath = realizationLogPath;
		this.resultsLogPath = resultsLogPath;
		this.runSettings = s;
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
	public void resetOpt(double[] vars) {
		for (int i = 0; i < vars.length; i++) {
			opt[i].forceCurrentValue(vars[i]);
		}
	}
	//startIndex refers to which variable to optimize first!
	public Variable[] optimizeVariables(String experimentName, int maxIter, int startIndex) {
		double currentScore = 0.0;
		double lastScore = currentScore;
		int currentIndex = startIndex;
		int currentIter = 1; 
		boolean stillImproving = true;
		String iterName;
		while (true) {
			if (currentIter > maxIter) {
				break;
			}
			iterName = experimentName+"-i"+String.format("%04d", currentIter);
			lastScore = currentScore;
			try {
				currentScore = performRun(iterName);
			}
			catch (IOException e) {
				e.printStackTrace();
				System.err.println("Error when logging!!");
			}
			boolean goodStep = currentScore <= lastScore;
			if (currentScore < lastScore) { //making strictly less will let it terminate a bit faster...
				stillImproving = true;
			}
			boolean stillMoving;
			try {
				stillMoving = opt.length == 0 ? false : opt[currentIndex].step(goodStep);
			}
			catch (Exception e) {
				e.printStackTrace();
				System.err.println("Inconsistent settlement!!");
				break;
			}
			if (!stillMoving && currentIndex < opt.length - 1) {
				currentIndex++;
			}
			else if (!stillMoving && !stillImproving) {
				break;
			}
			else {
				//unsettle everything, and set stillImproving to false to make sure it improves again in the next superiter
				for (Variable v : opt) {
					v.unsettle();
					currentIndex = 0;
				}
				stillImproving = false;
			}			
			currentIter++;
//			//if it returns true, we just proceed with the incremented variable!
		}
		System.out.println("Score: "+currentScore);
		System.out.println("Completeness: "+eval.getCompleteness());
		return this.opt;
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
	private String actrVarToString() {
		String s = "";
		if (runSettings.getModelType() == ModelType.ACTR) {
			s += ("negD: " + opt[0].getCurrentValue()+"; ");
			s += ("exp_year: "+opt[1].getCurrentValue()+"; ");
			s += ("pc_speaking: "+opt[2].getCurrentValue()+";");
			s += ("k: "+opt[3].getCurrentValue()+";");
		}
		return s;
		
	}
	
	private double performRun(String runName) throws IOException {	
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
		FileWriter fw = new FileWriter(resultsLogPath+runName);
		fw.write(this.runSettings.toString()+"\n");
		fw.write(actrVarToString()+"\n");
		fw.write(""+score);
		fw.close();
		return score;
	}
}
