package runconfig;


//temporary solution...
public class Consts {
	//constant input paths based on the folder setup
	protected static final String configPath = Settings.basePath+"actup-production/actup-realizer/config/";
	protected static final String grammarPath = configPath+"grammar/grammar.xml";	
	protected static final String experimentBasePath = Settings.basePath+"ap-largefiles/experiment/"; //base Experiment path
	protected static final String inputPath = experimentBasePath+"swbd-parsed/";
	protected static final String goldPath = experimentBasePath+"gold/";
	
	//constant output paths based on the current experimental paradigm
	protected static final String trialRealizationBaseDir = experimentBasePath + "trial-realizations/";
	protected static final String trialOutputPath = experimentBasePath+"output/"+Settings.trialSet+"/"; //output folder for the trial
	protected static final String trialRealizationSetPath = trialRealizationBaseDir+Settings.trialSet+"/"; //where the set of realization folders should be
}
