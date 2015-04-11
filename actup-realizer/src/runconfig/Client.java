package runconfig;

import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import optimization.ValleyClimber;
import optimization.Variable;
import optimization.VariableSet;

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
			System.err.println("You are overwriting a run!!!");
		}
		String logRealizations = IOSettings.logRealizations ? Consts.trialRealizationSetPath : null;
		String goldPath = IOSettings.loadGoldFromFile ? Consts.goldPath : null;
		ValleyClimber hc = new ValleyClimber(s, rs, Consts.grammarPath, Consts.inputPath, logRealizations, Consts.trialOutputPath, goldPath);
		
		for (int j = 0; j <= IOSettings.NumRandomRestarts; j++) {
			ExecutorService es = Executors.newCachedThreadPool();
			for (int i = 0; i < IOSettings.NumConcurrentStarts; i++) {
				es.execute(new optThread(opt, hc, j*IOSettings.NumConcurrentStarts+i));
			}
			es.shutdown();
			es.awaitTermination(12, TimeUnit.HOURS);
		}
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
		opt.randomAll();
		hc.optimizeVariables("e"+String.format("%02d", threadNum), opt, IOSettings.iterCap);		
	}
}