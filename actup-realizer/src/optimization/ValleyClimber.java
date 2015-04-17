package optimization;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Random;
import java.util.ArrayList;
import java.util.List;
import java.util.Arrays;

import ngrams.ACTRNgramModel;
import realize.Realization;
import realize.RealizeMain;
import realize.Realizer;
import runconfig.IOSettings;
import runconfig.ModelType;
import evaluation.Evaluator;
import evaluation.LevenshteinDistance;
import evaluation.LevenshteinDistanceWord;
import evaluation.Rouge;
import evaluation.Evaluation;

//a hillclimber... that likes valleys
public class ValleyClimber implements Optimizer {
	private Queue<File> inputFiles;
	private String realizationLogPath;
	private String resultsLogPath;
	private IOSettings runSettings;
	private Evaluator eval;
	private RealizeMain r;
	
	public ValleyClimber(IOSettings s, RealizeMain r, String inputFileDir, String resultsLogPath, double percentFiles, String realizationLogPath) {
		inputFiles = this.getInputFiles(inputFileDir, percentFiles);
		
		this.realizationLogPath = realizationLogPath;
		this.resultsLogPath = resultsLogPath;
		this.runSettings = s;
		this.r = r;
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
	private Queue<File> getInputFiles(String dir, double percentToUse) {
		File[] allInput = new File(dir).listFiles();
		int numFilesToUse = Math.max(1, (int) Math.round((double)allInput.length * percentToUse));
		Queue<File> out = new LinkedList<File>();
		Random r = new Random();
		for (int i = 0; i < numFilesToUse; i++) {
			out.offer(allInput[r.nextInt(allInput.length)]);
		}
		return out;
	}
	
	//experiment name should be a specific run of an experiment... which should be independent of the settings
	//and only dependent on opt
	public VariableSet optimizeVariables(String experimentName, VariableSet opt, int maxIter) {
		Realizer real = r.createNewRealizer();
		FileWriter fw = null;
		try {
			//it's for obvious reasons very important that no one holds the same experiment name in a concurrent setup!!!
			fw = new FileWriter(resultsLogPath+experimentName, true);
			fw.write(this.runSettings.toString()+"\n");
			for (File f : inputFiles) {
				fw.write(this.parseRunNum(f.toPath())+",");
			}
			fw.write("\n");
		}
		catch (IOException io) {
			io.printStackTrace();
			System.err.println("Unable to initialize realizer!");
			System.exit(1);
		}
		double currentScore = Double.MAX_VALUE; //the first run has to be an improvement!
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
			
			currentScore = performRun(real, iterName, opt, fw);

			//making strictly less will let it terminate a bit faster, but will explore less values
			boolean goodStep = currentScore < lastScore; 
			if (goodStep) { opt.acknowledgeImprovement(); }
			boolean stillMoving = opt.step(goodStep); //this checks if the variable itself is still improving, always true if first run
			if (!opt.updateIndex(stillMoving)) {
				break;
			}
			currentIter++;
			System.out.println("Experiment: "+experimentName+"; iter: "+currentIter+"Score: "+currentScore);
		}
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
			s += ("pc_speaking: "+d[ACTRNgramModel.psp_index]+"; ");
			s += ("k: "+d[ACTRNgramModel.k_index]+"; ");
		}
		return s;
		
	}
	
	private synchronized Queue<File> copyInput() {
		return new LinkedList<File>(inputFiles);
	}
	
	private double performRun(Realizer real, String runName, VariableSet opt, FileWriter fw)  {	
		//String num = in.getName().split("-")[1];
		System.out.println("This realization is a: " + runSettings.toString());
		

		//locking mechanism means no thread should ever be trying to realize the same
		Queue<File> localInput = this.copyInput(); //deepcopy
		List<Realization> realizations = new ArrayList<Realization>();
		while (!localInput.isEmpty()) { 
			File in = localInput.peek(); 
			String num = parseRunNum(in.toPath());
			if (!r.attemptAcquireLock(num)) {
				localInput.offer(localInput.poll());
				continue;
			}
			System.out.println("Beginning run on file: "+num);
			localInput.poll();
			
			String iterName = runName + "-f" + num;
			System.out.println("Beginning to realize: "+iterName);
			try {
				String realizationLogPath = this.realizationLogPath == null ? null : this.realizationLogPath+iterName+".spl";
				realizations.addAll(Arrays.asList(r.realize(r.setLM(real.getGrammar(), runSettings, opt, num), real, in.getPath().toString(), realizationLogPath)));
			}
			catch (Exception e) {
				e.printStackTrace();
				System.err.println("Error while realizing");
				System.exit(1);
			}
			r.releaseLock(num);
		}
		eval.loadData(realizations);
		Evaluation e = eval.scoreAll();
		try {
			writeout(fw, runName, e.getScore(), e.getCompleteness(), opt);
		}
		catch (IOException io) {
			io.printStackTrace();
			System.err.println("Error writing output");
			System.exit(1);
		}
		
		return e.getScore();
	}
	//this shouldn't need to be synchronized because every name that's being written to should be separate... but be careful!!
	private void writeout(FileWriter fw, String runName, double score, double completeness, VariableSet opt) throws IOException {
		fw.write(runName+":: "+"score: "+score+"; completeness: "+completeness+"; "+actrVarToString(opt)+"\n");
		fw.flush();
	}
}
