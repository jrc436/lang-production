package runconfig;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import optimization.ValleyClimber;
import optimization.Variable;
import optimization.VariableSet;
import realize.RealizeMain;

public class Client {	
	public static void main(String[] args) throws Exception {		
		VariableSet opt = new VariableSet(new Variable[0], 0);
		IOSettings s = new IOSettings();
		RealizationSettings rs = new RealizationSettings();
		
		if (s.getModelType() == ModelType.ACTR) {
			opt = IOSettings.actr_opt;
		}
		try {
			if (IOSettings.logRealizations) { Files.createDirectory(Paths.get(Consts.trialRealizationSetPath)); }
			Files.createDirectory(Paths.get(Consts.trialOutputPath));
		}
		catch (FileAlreadyExistsException ex) {
			System.err.println("You are potentially overwriting a run!!!");
		}
		String logRealizations = IOSettings.logRealizations ? Consts.trialRealizationSetPath : null;
		ValleyClimber hc = new ValleyClimber(s, new RealizeMain(rs, Consts.grammarPath, IOSettings.useCache), Consts.inputPath, Consts.trialOutputPath, IOSettings.percentInput, logRealizations);
		
		
		int baseNumTasks = IOSettings.interestingValues.size() / IOSettings.NumConcurrentStarts;
		int numStragglers = IOSettings.interestingValues.size() % IOSettings.NumConcurrentStarts;
		
		ExecutorService es = Executors.newCachedThreadPool();
		for (int i = 0; i < IOSettings.NumConcurrentStarts; i++) {
			Queue<double[]> interestingTasks = new LinkedList<double[]>();
			
			int numTasks = baseNumTasks;
			//since there are guaranteed to be less stragglers than threads mathematically, this will always work
			if (numStragglers > 0) {
				numTasks++;
				numStragglers--;
			}

			for (int j = 0; j < numTasks; j++) {
				interestingTasks.offer(IOSettings.interestingValues.poll());
			}
			es.execute(new optThread(opt, hc, i, interestingTasks));
		}
		es.shutdown();
		es.awaitTermination(12, TimeUnit.HOURS);
		
		//once this completes, realistically, we can do random restarts forever.
	}
}
class optThread implements Runnable {
	VariableSet opt;
	ValleyClimber hc;
	int threadNum;
	Queue<double[]> params;
	public optThread(VariableSet opt, ValleyClimber hc, int threadNum, Queue<double[]> params) {
		this.opt = new VariableSet(opt); //deep copy
		this.hc = hc;
		this.threadNum = threadNum;
		this.params = params == null ? new LinkedList<double[]>() : params;
		if (IOSettings.RunsPerThread < params.size()) {
			System.err.println("Warning!!!! Not all interesting Tasks will be executed on thread: "+threadNum);
		}
		
	}
	public void run() {
		for (int i = 0; i < IOSettings.RunsPerThread; i++) {
			if (params.peek() == null) {
				opt.randomAll();
			}
			else {
				opt.setWithDoubleArray(params.poll());
			}
			try {
				hc.optimizeVariables("e"+String.format("%02d", i*IOSettings.NumConcurrentStarts+threadNum), opt, IOSettings.iterCap);
			}
			catch (IOException io) {
				io.printStackTrace();
				System.err.println("Thread: "+threadNum+" encountered an IO Exception");
				System.exit(1);
			}
		}
	}
}