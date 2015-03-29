package opennlp.ccg;

import opennlp.ccg.optimization.Variable;


//temporary solution...
public class Settings {
	//private static final String basePath = "/Users/jrc/Public/jrc-research/";
	protected static final String basePath = "/work/research/"; //basepath refers to where the research dir is located
	protected static final String trialSet = "test-temp"; //trialset refers to a single optimization run's name
	protected static final int iterCap = 1; //number of iterations before it terminates
	protected static final int startIndex = 0;
	protected static final ModelType mType = ModelType.Random;
	protected static final TrainingSet trSet = TrainingSet.SWBD;
	protected static final EvaluationType eval = EvaluationType.EditDistanceChar;
	protected static final ScoringStrategy strat = ScoringStrategy.AdjScoreComplete;
	protected static final boolean logRealizations = false;
	protected static final boolean loadGoldFromFile = false;
	
	private static final Variable initialNegD = new Variable(-0.5, -0.95, -0.05, 0.05);
	private static final Variable initialExposureYears = new Variable(15.0, 1.0, 30.0, 1.0);
	private static final Variable initPCSpeaking = new Variable(0.25, 0.05, 0.65, 0.05);
	private static final Variable initK = new Variable(10.0, 1.0, 20.0, 1.0);
	protected static final Variable[] actr_opt = new Variable[] { initialNegD, initialExposureYears, initPCSpeaking, initK };
	
	private ModelType mt;
	private EvaluationType et;
	private TrainingSet ts;
	private ScoringStrategy ss;
	protected Settings(ModelType mt, EvaluationType et, TrainingSet ts, ScoringStrategy ss) {
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
