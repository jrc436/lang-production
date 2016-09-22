package util.sys;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

//k and v should obviously be the same classes are inp and out... just poop
public class Executor<J extends FileProcessor<K, V>, K extends DataType, V extends DataType> {
	private final BlockingQueue<String> messages;
	private final int gbPerThread;
	private final String name;
	private final Class<K> in;
	private final Class<J> fp;
	private final Class<V> out;
	private J proc;
	public Executor(String name, int gbPerThread, Class<J> fp, Class<K> inp, Class<V> out) {
		this.name = name;
		this.gbPerThread = gbPerThread;
		this.messages = new LinkedBlockingQueue<String>();
		this.in = inp;
		this.out = out;
		this.fp = fp;
	}
	public void initializeFromCmdLine(String[] cmdArgs) {
		InputParse ip = new InputParse(cmdArgs);
		InputProcessor<J, K, V> ipr = new InputProcessor<J, K, V>(fp, in, out, ip);
		if (cmdArgs.length < 2) {
			ipr.createError();
		}
		String[][] args = new String[3][];	
		int counts[] = new int[3];
		boolean copiedArgs = false;
		int count = 0;
		for (int i = 0; i < 3; i++) {
			int oldCount = count;
			//the "reuse" case is obviously pretty minor... but still best way to do this
			if (i == 2 && count == ip.getRemaining().length && in.equals(out)) {
				args[i] = Arrays.copyOf(args[i-1], args[i-1].length);
				copiedArgs = true;
			}
			else {
				args[i] = ipr.getArgs(count, i);
			}
			count += args[i].length;	
			if (count < ip.getRemaining().length && ipr.indexIsBound(count)) {
				count++;
			}
			counts[i] = count - oldCount;
		}	
		if (count != ip.getRemaining().length) {
			int fullcount = 2 + count;
			System.err.println("Arguments received: "+cmdArgs.length+". Process effectively parsed: "+fullcount);
			if (copiedArgs) {
				System.err.println("Warning: Args were copied from Input for Output, check for correctness");
			}
			System.err.println("Input and Output directories were successfully received"); //or it would've thrown an error earlier
			ipr.countErrorHandling(counts, args);
			ipr.createError();
		}
 		setProc(ip, ipr, args);
	}
	
	private void setProc(InputParse ip, InputProcessor<J, K, V> ipr, String[][] args) {
		List<String[]> goodArgs = new ArrayList<String[]>();
		for (int i = 0; i < args.length; i++) {
			if (args[i].length != 0) {
				goodArgs.add(args[i]);
			}
		}
		try {	
			if (goodArgs.size() == 0) {
				proc = fp.getConstructor(String.class, String.class).newInstance(ip.getInputPath(), ip.getOutputPath());
			}
			else if (goodArgs.size() == 1) {
				proc = fp.getConstructor(String.class, String.class, String[].class).newInstance(ip.getInputPath(), ip.getOutputPath(), goodArgs.get(0));
			}
			else if (goodArgs.size() == 2) {
				proc = fp.getConstructor(String.class, String.class, String[].class, String[].class).newInstance(ip.getInputPath(), ip.getOutputPath(), goodArgs.get(0), goodArgs.get(1));
			}
			else {
				proc = fp.getConstructor(String.class, String.class, String[].class, String[].class, String[].class).newInstance(ip.getInputPath(), ip.getOutputPath(), goodArgs.get(0), goodArgs.get(1), goodArgs.get(2));
			}
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			System.err.println("Error Creating Process. The most likely reason for this is that there is no accessible constructor of the type you requested.");
			System.err.println("This should be easily solvable by adding one. You requested a constructor that takes the input and output paths, along with: "+goodArgs.size()+" string arrays.");
			System.err.println("It could also be an error in the existing constructor, which will appear below.");
			e.printStackTrace();
			ipr.createError();
		} 
	}
	
	
	protected Executable getExecutable(int threadNum, BlockingQueue<String> messages) {//, V initialValue) {
		//return new ProcessWorker(threadNum, gbPerThread, messages, proc, initialValue);
		return new ProcessWorker(threadNum, messages);
	}
	
	//needs to be updated slightly. After N rounds of runs, N output files will be produced. 
	public void run() {
		Thread log;
		try {
            log = new Thread(new Logger(messages, name+".log"));
            log.setDaemon(true);
            log.start();
		} catch (IOException e) {
            System.err.println("Error initializing output. Check your output paths");
            System.exit(1);
		}
	    int numThr = ResourceAllocator.getSuggestedNumThreads(gbPerThread);
	    ExecutorService es = Executors.newCachedThreadPool();  
	    for (int i = 0; i < numThr; i++) {
	    	es.execute(getExecutable(i, messages));//, proc.getInitialThreadValue()));
	    }
	    es.shutdown();
	    try {
			es.awaitTermination(Long.MAX_VALUE, TimeUnit.DAYS);
		} catch (InterruptedException e) {
			System.err.println("Process interrupted: "+proc.toString()+", will attempt to write");
			e.printStackTrace();
		}
	    finally {
	    	messages.add("Writing process beginning.");
	    	proc.write();
	    	messages.add("Writing process complete. Process will now terminate");
	    }
	}
	class ProcessWorker extends Executable {
		//private J proc;
		//private K input;
		//private V output;
		private V threadAggregate;
		//private final int gbAlloc;
		public ProcessWorker(int threadnum, BlockingQueue<String> log) {//,int gbAlloc, J proc, V aggregate) {
			super(threadnum, log);
			//this.proc = proc;
			//this.gbAlloc = gbAlloc;
			this.threadAggregate = proc.getInitialThreadValue();
		}
//		private boolean shouldReduce() {
//			return this.threadAggregate.isFull(gbPerThread/2);
//		}
//		private void reInitializeThread() {
//			proc.reduce(threadAggregate);
//			this.threadAggregate = proc.getInitialThreadValue();
//		}
		@Override
		public void run() {
			this.logMessage("Thread"+getNum()+" is beginning its run");
			while (true) {
				this.logMessage("Thread"+getNum()+" is waiting to acquire another file. "+proc.numReadableRemaining()+ " files remain.");
//				if (shouldReduce()) {
//					this.logMessage("Thread"+getNum()+" has exceeded its recommended size and is reducing early.");
//					reInitializeThread();
//				}
				K data = proc.getNextData();						
				if (data == null) {
					this.logMessage("Thread"+getNum()+" has failed to acquire more data. It's ending its run.");
					break;
				}
				else {
					this.logMessage("Thread"+getNum()+" has acquired more data");		
				}
				proc.map(data, threadAggregate);
			}
			this.logMessage("Thread"+getNum()+" is beginning its reduce.");
			proc.reduce(threadAggregate);
			this.logMessage("Thread"+getNum()+" is exiting.");			
		}
	}
}
