package runconfig;


//temporary solution...
public class Consts {
	//realizer consts
	public static final int MAX_EDGE_LIMIT = Integer.MAX_VALUE; //worth noting that technically, the code doesn't require this to be the max value 
	public static final int PRUNE_LEAST_EDGES = Integer.MAX_VALUE; 
	
	
	//constant input paths based on the folder setup
	protected static final String configPath = IOSettings.basePath+"actup-production/actup-realizer/config/";
	protected static final String grammarPath = configPath+"grammar/grammar.xml";	
	protected static final String experimentBasePath = IOSettings.basePath+"ap-largefiles/experiment/"; //base Experiment path
	protected static final String inputPath = experimentBasePath+"swbd-parsed/";
	protected static final String goldPath = experimentBasePath+"gold/";
	
	//constant output paths based on the current experimental paradigm
	protected static final String trialRealizationBaseDir = experimentBasePath + "trial-realizations/";
	protected static final String trialOutputPath = experimentBasePath+"output/"+IOSettings.trialSet+"/"; //output folder for the trial
	protected static final String trialRealizationSetPath = trialRealizationBaseDir+IOSettings.trialSet+"/"; //where the set of realization folders should be
}
