package runconfig;

import ngrams.ACTRNgramModel;
import optimization.Variable;
import optimization.VariableSet;


//temporary solution...
public class IOSettings {
	//I/O Settings
	protected static final String basePath = "/Users/jrc/Public/jrc-research/";
	//protected static final String basePath = "/work/research/"; //basepath refers to where the research dir is located
	protected static final String trialSet = "memshare-climb-round3"; //trialset refers to a single optimization run's name
	protected static final int iterCap = 10000; //number of iterations before it terminates
	protected static final int NumConcurrentStarts = 8; //should be roughly equal to number of processors
	protected static final int NumRandomRestarts = 100;
	private static final int startIndex = ACTRNgramModel.ey_index;
	protected static final ModelType mType = ModelType.ACTR;
	protected static final TrainingSet trSet = TrainingSet.SWBD;
	protected static final EvaluationType eval = EvaluationType.EditDistanceWord;
	protected static final ScoringStrategy strat = ScoringStrategy.ScoreComplete;
	protected static final double percentInput = 0.02;
	
	protected static final boolean logRealizations = false;
	protected static final boolean loadGoldFromFile = false;
	
	
	private static final Variable initialNegD = new Variable(-0.5, -0.95, -0.05, 0.05);
	private static final Variable initialExposureYears = new Variable(15.0, 1.0, 30.0, 1.0);
	private static final Variable initPCSpeaking = new Variable(0.05, 0.05, 0.65, 0.05);
	private static final Variable initK = new Variable(3.0, 1.0, 20.0, 1.0);
	protected static final VariableSet actr_opt;
	static {
		Variable[] actr = new Variable[4];
		actr[ACTRNgramModel.ey_index] = initialExposureYears;
		actr[ACTRNgramModel.k_index] = initK;
		actr[ACTRNgramModel.negD_index] = initialNegD;
		actr[ACTRNgramModel.psp_index] = initPCSpeaking;
		actr_opt = new VariableSet(actr, startIndex);
	}
	
	private ModelType mt;
	private EvaluationType et;
	private TrainingSet ts;
	private ScoringStrategy ss;
	protected IOSettings() {
		this.mt = mType;
		this.et = eval;
		this.ts = trSet;
		this.ss = strat;
	}
	protected IOSettings(ModelType mt, EvaluationType et, TrainingSet ts, ScoringStrategy ss) {
		this.mt = mt;
		this.et = et;
		this.ts = ts;
		this.ss = ss;
	}
	public ScoringStrategy getScoringStrategy() {
		return this.ss;
	}
	public ModelType getModelType() {
		return this.mt;
	}
	public EvaluationType getEvaluationType() {
		return this.et;
	}
	public TrainingSet getTrainingSet() {
		return this.ts;
	}
	public String toString() {
		return "Model: "+mt.toString()+"; Eval: "+et.toString()+"; Train: "+ts.toString()+"; "+ss.toString();
	}
}
