package opennlp.ccg;

import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Paths;

import opennlp.ccg.optimization.ValleyClimber;
import opennlp.ccg.optimization.Variable;

public class Client {	
	
	
	public static void main(String[] args) throws Exception {
		Variable[] opt = new Variable[0];
		Settings s = new Settings(Settings.mType, Settings.eval, Settings.trSet, Settings.strat);
		
		if (Settings.mType == ModelType.ACTR) {
			//changing the order here could have serious consequences!!!!
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
		ValleyClimber hc = new ValleyClimber(s, Consts.grammarPath, opt, Consts.inputPath, logRealizations, Consts.trialOutputPath, goldPath);
		hc.optimizeVariables(Settings.trialSet, Settings.iterCap, Settings.startIndex);
	}
	
	
}