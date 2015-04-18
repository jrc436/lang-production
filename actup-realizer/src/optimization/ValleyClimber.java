package optimization;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Random;

import realize.Realization;
import realize.RealizeMain;
import realize.Realizer;
import runconfig.IOSettings;
import evaluation.Evaluation;
import evaluation.Evaluator;
import evaluation.LevenshteinDistance;
import evaluation.LevenshteinDistanceWord;
import evaluation.Rouge;

//a hillclimber... that likes valleys
public class ValleyClimber implements Optimizer {
	private Queue<File> inputFiles;
	private String realizationLogPath;
	private String resultsLogPath;
	private IOSettings runSettings;
	private Evaluator eval;
	private RealizeMain r;
	private HashSet<RunData> prevRuns;
	
	public ValleyClimber(IOSettings s, RealizeMain r, String inputFileDir, String resultsLogPath, double percentFiles, String realizationLogPath) {
		inputFiles = this.getInputFiles(inputFileDir, percentFiles);
		
		prevRuns = new HashSet<RunData>();
		
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
		List<File> allInput = new ArrayList<File>(Arrays.asList(new File(dir).listFiles()));
		int numFilesToUse = Math.max(1, (int) Math.round((double)allInput.size() * percentToUse));
		Queue<File> out = new LinkedList<File>();
		Random r = new Random();
		for (int i = 0; i < numFilesToUse; i++) {
			//no repeats!
			int fileIndex = r.nextInt(allInput.size());
			out.offer(allInput.get(fileIndex));
			allInput.remove(fileIndex);
		}
		return out;
	}
	
	//experiment name should be a specific run of an experiment... which should be independent of the settings
	//and only dependent on opt
	public VariableSet optimizeVariables(String experimentName, VariableSet opt, int maxIter) throws IOException{
		Realizer real = r.createNewRealizer();
		FileWriter fw = null;
			//it's for obvious reasons very important that no one holds the same experiment name in a concurrent setup!!!
		fw = new FileWriter(resultsLogPath+experimentName, true);
		fw.write(this.runSettings.toString()+"\n");
		for (File f : inputFiles) {
			fw.write(this.parseRunNum(f.toPath())+",");
		}
		fw.write("\n");
		
		int currentIter = 1;
		String iterName;
		RunData bestRun = new RunData(opt);
		System.out.println("%%% "+experimentName+":: is a "+runSettings.toString()+"%%%");
		while (true) {
			if (currentIter > maxIter) {
				fw.write("Reached iter limit");
				break;
			}
			System.out.println("%%% "+experimentName+":: Iteration "+currentIter+"/"+maxIter+"%%%");
			iterName = experimentName+"-i"+String.format("%04d", currentIter);
			
			//lastScore = currentScore;
			Evaluation done = getRun(new RunData(opt));
			RunData newRun;
			if (done == null) {
				newRun = new RunData(opt, performRun(real, iterName, opt), iterName);
				addRunToRuns(newRun);
			}
			else {
				newRun = new RunData(opt, done, iterName);
			}
			fw.write(newRun.toString() + "\n");
			fw.flush();
			
			System.out.println("%%%"+newRun.toString()+"%%%");
			
			boolean goodStep = false;
			if (newRun.improvement(bestRun)) {
				bestRun = newRun;
				opt.acknowledgeImprovement();
				goodStep = true; //we only care if it's an improvement over our current best run.
			}
			//this checks if the variable itself is still improving, always true if first run
			boolean stillMoving = opt.step(goodStep); 
			if (!stillMoving && !opt.updateIndex()) { //note that updateIndex will only be called if stillmoving is false
				fw.write("Breaking early as all variables have been optimized.");
				break;
			}
			currentIter++;
		}
		fw.close();
		return opt;
	}
	private synchronized void addRunToRuns(RunData run) {
		prevRuns.add(run);
	}
	private synchronized Evaluation getRun(RunData run) {
		return prevRuns.contains(run) ? run.getEval() : null;
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
	
	private synchronized Queue<File> copyInput() {
		return new LinkedList<File>(inputFiles);
	}
	
	private Evaluation performRun(Realizer real, String runName, VariableSet opt)  {	
		//String num = in.getName().split("-")[1];
		
		
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
			System.out.println("%%% "+runName+" is beginning run on file: "+num+"%%%");
			localInput.poll();
			
			String iterName = runName + "-f" + num;
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
		Evaluation e = null;
		synchronized(eval) {
			eval.loadData(realizations);
			e = eval.scoreAll();
		}
		return e;
	}
}
