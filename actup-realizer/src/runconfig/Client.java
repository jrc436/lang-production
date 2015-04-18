package runconfig;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Paths;
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
		
		ExecutorService es = Executors.newCachedThreadPool();
		for (int i = 0; i < IOSettings.NumConcurrentStarts; i++) {
			es.execute(new optThread(opt, hc, i));
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
	public optThread(VariableSet opt, ValleyClimber hc, int threadNum) {
		this.opt = new VariableSet(opt); //deep copy
		this.hc = hc;
		this.threadNum = threadNum;
	}
	public void run() {
		for (int i = 0; i <= IOSettings.NumRandomRestarts; i++) {
			opt.randomAll();
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