package runconfig;

import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Paths;

import optimization.ValleyClimber;
import optimization.Variable;
import optimization.VariableSet;

public class Client {	
	
	
	public static void main(String[] args) throws Exception {
		VariableSet opt = new VariableSet(new Variable[0], 0);
		Settings s = new Settings(Settings.mType, Settings.eval, Settings.trSet, Settings.strat);
		
		if (Settings.mType == ModelType.ACTR) {
			opt = Settings.actr_opt;
		}
		try {
			if (Settings.logRealizations) { Files.createDirectory(Paths.get(Consts.trialRealizationSetPath)); }
			Files.createDirectory(Paths.get(Consts.trialOutputPath));
		}
		catch (FileAlreadyExistsException ex) {
			System.err.println("You are overwriting a run!!!");
		}
		String logRealizations = Settings.logRealizations ? Consts.trialRealizationSetPath : null;
		String goldPath = Settings.loadGoldFromFile ? Consts.goldPath : null;
		ValleyClimber hc = new ValleyClimber(s, Consts.grammarPath, Consts.inputPath, logRealizations, Consts.trialOutputPath, goldPath);
		for (int i = 0; i < Settings.NumConcurrentStarts; i++) {
			opt.randomAll();
			hc.optimizeVariables("e"+String.format("%02d", i), opt, Settings.iterCap);		
		}
	}
	
	
}