package optimization;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.BlockingQueue;

import ngrams.AbstractStandardNgramModel;
import realize.Realization;
import realize.RealizeMain;
import realize.Realizer;
import runconfig.BeginMessage;
import runconfig.EndMessage;
import runconfig.IOSettings;
import runconfig.InputStruct;
import runconfig.Message;
import runconfig.ResultMessage;
import runconfig.ThreadState;
import evaluation.Evaluation;
import evaluation.Evaluator;
import evaluation.LevenshteinDistance;
import evaluation.LevenshteinDistanceWord;
import evaluation.Rouge;

//a hillclimber... that likes valleys
public class ValleyClimber implements Optimizer {

	private Evaluator eval;
	private RealizeMain r;
	
	private HashSet<RunData> prevRuns;
	private BlockingQueue<Message> resultQ;
	private BlockingQueue<String> log;
	private final IOSettings io;
	private final int timeoutNum;
	
	public ValleyClimber(RealizeMain r, IOSettings io, BlockingQueue<Message> resultQ, BlockingQueue<String> log) {
		this.resultQ = resultQ;
		this.log = log;
		prevRuns = new HashSet<RunData>();
		this.r = r;
		this.io = io;
		switch (io.getEvaluationType()) {
			case EditDistanceChar:
				eval = new LevenshteinDistance(io.getScoringStrategy());
				break;
			case EditDistanceWord:
				eval = new LevenshteinDistanceWord(io.getScoringStrategy());
				break;
			case ROUGE:
				eval = new Rouge(io.getScoringStrategy(), io.getEvaluationType().extPath());
				break;
		}
		switch (io.getTrainingSet()) {
			case SWBD10FOLD:
				timeoutNum = 10;
				break;
			case SWBDM1:
				timeoutNum = 650;
				break;
			default:
				timeoutNum = 1;
				break;
		}
	}
	
	
	//experiment name should be a specific run of an experiment... which should be independent of the settings
	//and only dependent on opt
	public VariableSet optimizeVariables(String experimentName, VariableSet opt, int maxIter, ThreadState ts) {
		Realizer real = r.createNewRealizer();
		
			//it's for obvious reasons very important that no one holds the same experiment name in a concurrent setup!!!
		long timeNS = System.nanoTime();		
		int currentIter = 1;
		String iterName;
		RunData bestRun = new RunData(opt);
		try {
			resultQ.put(new BeginMessage(io, r.getRunFiles(), experimentName));
			while (true) {
				if (currentIter > maxIter) {
					log.offer(experimentName+" Reached iter limit");
					log.offer(experimentName+" Best: " + bestRun.toString());
					log.offer(experimentName+" Total Elapsed Time: " + (System.nanoTime() - timeNS));
					resultQ.put(new EndMessage(bestRun, experimentName));
					break;
				}
				log.offer("%%% "+experimentName+":: Iteration "+currentIter+"/"+maxIter+"%%%");
				iterName = experimentName+"-i"+String.format("%04d", currentIter);
				
				//lastScore = currentScore;
				Evaluation done = getRun(new RunData(opt));
				RunData newRun;
				if (done == null) {
					newRun = new RunData(r.getRunFiles(), opt, performRun(real, iterName, opt, ts), iterName, System.nanoTime() - timeNS);
					resultQ.put(new ResultMessage(newRun, experimentName));
					addRunToRuns(newRun);
				}
				else {
					log.offer("%%% "+experimentName+" This run has been done before. Loading! %%%");
					newRun = new RunData(r.getRunFiles(), opt, done, iterName, System.nanoTime() - timeNS);
					resultQ.put(new ResultMessage(newRun, experimentName));
				}
				log.offer(experimentName+": "+newRun.toString());
				
				boolean goodStep = false;
				if (newRun.improvement(bestRun)) {
					bestRun = newRun;
					opt.acknowledgeImprovement();
					goodStep = true; //we only care if it's an improvement over our current best run.
				}
				//this checks if the variable itself is still improving, always true if first run
				boolean stillMoving = opt.step(goodStep); 
				if (!stillMoving && !opt.updateIndex()) { //note that updateIndex will only be called if stillmoving is false
					log.offer(experimentName+ " Breaking early as all variables have been optimized.");
					log.offer(experimentName+ " Best: " + bestRun.toString());
					resultQ.put(new EndMessage(bestRun, experimentName));
					log.offer(experimentName+" Total Elapsed Time: " + (System.nanoTime() - timeNS));
					break;
				}
				currentIter++;
				ts.completeIter();
			}
		}
		catch (InterruptedException ie) {
			System.err.println("Experiment "+experimentName+" was cancelled before finishing. Please in the future use quit to close gracefully.");
		}
		return opt;
	}
	private synchronized void addRunToRuns(RunData run) {
		prevRuns.add(run);
	}
	private Evaluation getRun(RunData run) {
		return prevRuns.contains(run) ? run.getEval() : null;
	}
	
	private Evaluation performRun(Realizer real, String runName, VariableSet opt, ThreadState ts) {	
		List<Realization> realizations = new ArrayList<Realization>();
		Set<Integer> alreadyHad = new HashSet<Integer>();
		List<Integer> completedFiles = new ArrayList<Integer>();
		
		while (true) {
			int lock = r.attemptAcquireLock(alreadyHad, runName, log, timeoutNum);
			if (lock == RealizeMain.NO_LOCK_REMAINING) {
				break;
			}
			log.offer(runName+" successfully acquired lock:"+lock);
			Queue<InputStruct[]> inp = r.getInput(lock);
			AbstractStandardNgramModel scorer = r.getScorer(lock, opt.getDoubleArray());
			ts.startLock(lock, inp.size());
			//at this point, we clearly have a lock
			
			while (!inp.isEmpty()) {
				InputStruct[] is = inp.poll();
				int filenum = is[0].getFileNum(); //all of the filenums in the array are the same, actually
				log.offer("%%% "+runName+" is beginning run on file: "+filenum+"%%%");
				ts.startFile(filenum);
				realizations.addAll(Arrays.asList(r.realize(scorer, real, is, log, runName)));
				completedFiles.add(filenum);
				ts.handleSaving(runName, realizations, completedFiles, opt, -1);
				scorer.clean();
				ts.completeFile(filenum);
			}
			r.releaseLock(lock);
			ts.completeLock(lock);
		}
		return eval.scoreAll(realizations);
	}
}
