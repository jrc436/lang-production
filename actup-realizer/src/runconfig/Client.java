package runconfig;

import grammar.Grammar;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
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
		BlockingQueue<String> progress = new LinkedBlockingQueue<String>();
		BlockingQueue<Message> results = new LinkedBlockingQueue<Message>();
		
		Grammar g = new Grammar(new File(Consts.grammarPath).toURI().toURL());
		LMHandler score = new LMHandler(g, IOSettings.mType, IOSettings.trSet, opt);
		InputHandler inp = new InputHandler(g, Consts.inputPath, IOSettings.trSet, IOSettings.percentInput);
		
		RealizeMain rm = new RealizeMain(rs, g, inp, score);
		ValleyClimber hc = new ValleyClimber(rm, s, results, progress);				
		
		int baseNumTasks = IOSettings.interestingValues.size() / IOSettings.NumConcurrentStarts;
		int numStragglers = IOSettings.interestingValues.size() % IOSettings.NumConcurrentStarts;
		
		ExecutorService es = Executors.newCachedThreadPool();		
		es.execute(new Logger(progress, Consts.logPath));
		
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
			es.execute(new OptTask(opt, hc, i, interestingTasks));
		}
		es.shutdown();
		es.awaitTermination(12, TimeUnit.HOURS);
	}
	public static int getThreadNum(String expName) {
		String firstpart = expName.split("-")[0];
		return Integer.parseInt(firstpart.substring(1, firstpart.length()));
	}
}
class ReportResults implements Runnable {
	private final BlockingQueue<Message> results;
	private final FileWriter[] cachedfw;
	public ReportResults(BlockingQueue<Message> results) {
		cachedfw = new FileWriter[IOSettings.NumConcurrentStarts];
		this.results = results;
	}
	public void run() {
		while (true) {
			if (!results.isEmpty()) {		
				Message r = results.poll();
				int tn = Client.getThreadNum(r.getExpName());
				try {								
					if (r instanceof BeginMessage) {
						cachedfw[tn] = new FileWriter(IOSettings.trialSet+r.getExpName(), true);						
					}			
					cachedfw[tn].write(r.print()+"\n");
					if (r instanceof EndMessage) {
						cachedfw[tn].close();
					}
				} 
				catch (IOException e) {
					e.printStackTrace();
					System.err.println("Error printing from thread: "+tn);
				}
			}
		}
	}
	
}
class Logger implements Runnable {
	private final BlockingQueue<String> messages;
	private final FileWriter fw;
	public Logger(BlockingQueue<String> messages, String fp) throws IOException {
		this.messages = messages;
		
		fw = new FileWriter(new File(fp));
	}
	public void run() {
		while (true) {
			if (!messages.isEmpty()) {
				try {
					fw.write(messages.poll()+"\n");
					fw.flush();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}
}
class OptTask implements Runnable {
	VariableSet opt;
	ValleyClimber hc;
	int threadNum;
	Queue<double[]> params;
	public OptTask(VariableSet opt, ValleyClimber hc, int threadNum, Queue<double[]> params) {
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
			hc.optimizeVariables("e"+threadNum+"-"+i, opt, IOSettings.iterCap);		
		}
	}
}