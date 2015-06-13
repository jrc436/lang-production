package runconfig;

import grammar.Grammar;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Scanner;
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
		
		Thread log = new Thread(new Logger(progress, Consts.logPath));
		log.setDaemon(true);
		log.start();
		
		Thread r = new Thread(new ReportResults(results));
		r.setDaemon(true);
		r.start();
				
		List<ThreadState> ts = new ArrayList<ThreadState>();
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
			ThreadState t = new ThreadState(results, s, rm.totalNumLocks(), IOSettings.iterCap);
			es.execute(new OptTask(opt, hc, i, interestingTasks, t));
			ts.add(t);
		}
		Thread inpt = new Thread(new InputClient(ts, r, log));
		inpt.setDaemon(true);
		inpt.start();
		
		es.shutdown();
		es.awaitTermination(48, TimeUnit.HOURS);
	}
	public static int getThreadNum(String expName) {
		String firstpart = expName.split("-")[0];
		return Integer.parseInt(firstpart.substring(1, firstpart.length()));
	}
	public static String getFormattedDate() {
		String[] parts = new Date().toString().split(" ");
		String date = parts[0]+"-"+parts[3];
		return date;
	}
}
class InputClient implements Runnable {
	private final Scanner in;
	private final List<ThreadState> ts;
	private final Thread prog;
	private final Thread log;
	public InputClient(List<ThreadState> ts, Thread prog, Thread log) {
		in = new Scanner(System.in);
		this.ts = ts;
		this.prog = prog;
		this.log = log;
	}
	public void run() {
		while (in.hasNextLine()) {
			String s = in.nextLine();
			InpComm.processCommand(ts, log, prog, s);
		}
	}
	private enum InpComm {
		progress,
		status,
		save,
		help,
		quit,
		other;
		private static String description(InpComm s) {
			switch(s) {
				case help:
					return "gives information about all the commands";
				case progress:
					return "prints the progress in the current task set, the remaining task sets, and the current experiment name for all threads";
				case quit:
					return "terminates the process gracefully";
				case save:
					return "saves the current progress in much the same way as it should save at termination";
				case status:
					return "prints the current job and the time of last activity for all threads";
				default:
					return null;			
			}
		}
		private static InpComm getEnum(String s) {
			InpComm retval;
			try {
				retval = InpComm.valueOf(s);
			}
			catch (IllegalArgumentException i) {
				retval = InpComm.other;
			}
			return retval;
		}
		private static void processCommand(List<ThreadState> ts, Thread prog, Thread log, String s) {
			InpComm ic = InpComm.getEnum(s);
			switch (ic) {
				case help:
					for (InpComm c : InpComm.values()) {
						if (c == InpComm.other) { continue; }
						System.out.println("  "+c.toString()+"      "+description(c));
					}
					break;
				case progress:
					for (int i = 0; i < ts.size(); i++) {
						System.out.println("Thread"+i+" :: "+ts.get(i).progress());
					}
					break;
				case quit:
					prog.interrupt();
					log.interrupt();
					System.out.println("Shutting down.");
					System.exit(0);
					break;
				case save:
					System.out.println("Saving... each thread will report when it's saved its current work, which may take some time.");
					for (int i = 0; i < ts.size(); i++) {
						ts.get(i).requestSave();
					}
					break;
				case status:
					for (int i = 0; i < ts.size(); i++) {
						System.out.println("Thread"+i+" :: "+ts.get(i).status());
					}
					break;
				default:
					System.out.println("Type help for more information");
					break;				
			}
		}
	}
}

class ReportResults implements Runnable {
	private final BlockingQueue<Message> results;
	private final FileWriter[] cachedfw;
	public ReportResults(BlockingQueue<Message> results) {
		cachedfw = new FileWriter[IOSettings.NumConcurrentStarts];
		this.results = results;
	}
	private void writeMessage(Message r) {
		int tn = Client.getThreadNum(r.getExpName());
		try {								
			if (r instanceof BeginMessage) {
				cachedfw[tn] = new FileWriter(Consts.trialOutputPath+r.getExpName(), true);
				System.out.println("Opening file writer for: "+r.getExpName()+" at "+Consts.trialOutputPath+r.getExpName());
			}			
			cachedfw[tn].write(r.print()+"\n");
			cachedfw[tn].flush();
			if (r instanceof ResultMessage) {
				System.out.println("Results saved on experiment: "+r.getExpName());
			}
			else if (r instanceof EndMessage) {
				cachedfw[tn].close();
				System.out.println("Closing file writer for: "+r.getExpName());
			}
		} 
		catch (IOException e) {
			e.printStackTrace();
			System.err.println("Error printing from thread: "+tn);
		}
	}
	public void run() {
		while (true) {
			try {
				writeMessage(results.take());			
			}
			catch (InterruptedException ie) {
				while (!results.isEmpty()) {
					writeMessage(results.poll());
				}
				for (FileWriter fw : cachedfw) {
					try { fw.close();
					} catch (IOException e) {}
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
			try {
				writeMessage(messages.take());
			} catch (InterruptedException e) {
				System.err.println("Logger has been interrupted. It has "+messages.size()+ " messages remaining at this time. Attempting to write.");
				while (!messages.isEmpty()) {
					writeMessage(messages.poll());
				}
				try { fw.close();
				} catch (IOException e1) { }
			}
		}
	}
	private void writeMessage(String s) {
		try {
			String date = Client.getFormattedDate();
			fw.write(date+"::"+s+"\n");
			fw.flush();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

}
class OptTask implements Runnable {
	private final VariableSet opt;
	private final ValleyClimber hc;
	private final int threadNum;
	private final Queue<double[]> params;
	private final ThreadState ts;
	public OptTask(VariableSet opt, ValleyClimber hc, int threadNum, Queue<double[]> params, ThreadState ts) {
		this.opt = new VariableSet(opt); //deep copy
		this.hc = hc;
		this.threadNum = threadNum;
		this.params = params == null ? new LinkedList<double[]>() : params;
		if (IOSettings.RunsPerThread < params.size()) {
			System.err.println("Warning!!!! Not all interesting Tasks will be executed on thread: "+threadNum);
		}
		this.ts = ts;
	}
	public void run() {
		for (int i = 0; i < IOSettings.RunsPerThread; i++) {
			if (params.peek() == null) {
				opt.randomAll();
			}
			else {
				opt.setWithDoubleArray(params.poll());
			}
			hc.optimizeVariables("e"+threadNum+"-"+i, opt, IOSettings.iterCap, ts);		
		}
	}
}