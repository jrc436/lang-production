package runconfig;

import java.util.LinkedList;
import java.util.Queue;

import ngrams.ACTRNgramModel;
import optimization.Variable;
import optimization.VariableSet;


//temporary solution...
public class IOSettings {
	//I/O Settings
	//protected static final String basePath = "/Users/jrc/Public/jrc-research/";
	protected static final String basePath = "/work/research/"; //basepath refers to where the research dir is located
	protected static final String trialSet = "cmd-test4"; //trialset refers to a single optimization run's name
	protected static final int iterCap = 1; //number of iterations before it terminates
	protected static final int NumConcurrentStarts = 2; //should be roughly equal to number of processors
	protected static final int RunsPerThread = 1;
	private static final int startIndex = ACTRNgramModel.ey_index;
	protected static final ModelType mType = ModelType.Ngram;
	protected static final TrainingSet trSet = TrainingSet.SWBD10FOLD;
	protected static final EvaluationType eval = EvaluationType.EditDistanceWord;
	protected static final ScoringStrategy strat = ScoringStrategy.ScoreAll;
	protected static final double percentInput = 0.3;
	
	//values that should started at in any run. Set in the static block
	protected static final Queue<double[]> interestingValues = new LinkedList<double[]>(); 	
	
	private static final Variable negD = new Variable("-d", -0.5, -1.0, -0.0, 0.05);
	private static final Variable expYears = new Variable("Exposure Years", 15.0, 1.0, 30.0, 1.0);
	private static final Variable pcSpeaking = new Variable("Percent Speaking", 0.3, 0.05, 0.65, 0.05);
	private static final Variable k = new Variable("k", 3.0, 0.0, 20.0, 1.0);
	protected static final VariableSet actr_opt;
	static {
		Variable[] actr = new Variable[Consts.actr_length];
		actr[ACTRNgramModel.ey_index] = expYears;
		actr[ACTRNgramModel.k_index] = k;
		actr[ACTRNgramModel.negD_index] = negD;
		actr[ACTRNgramModel.psp_index] = pcSpeaking;
		actr_opt = new VariableSet(actr, startIndex);
		
		//offerInteresting(25.0, 0.0, -0.5, 0.35);
		offerInteresting(25.0, 1.0, -0.5, 0.35);
		offerInteresting(25.0, 3.0, -0.5, 0.35);
		offerInteresting(25.0, 10.0, -0.5, 0.35);
		
		//offerInteresting(1.0, 3.0, -0.5, 0.35);
		//offerInteresting(10.0, 3.0, -0.5, 0.35);
		
		offerInteresting(25.0, 5.0, 0.0, 0.35);
		offerInteresting(25.0, 5.0, -1.0, 0.35);
		offerInteresting(25.0, 5.0, -0.16, 0.35);
	}
	private static void offerInteresting(double ey, double k, double negD, double psp) {
		double[] interesting = new double[Consts.actr_length];
		interesting[ACTRNgramModel.ey_index] = ey;
		interesting[ACTRNgramModel.k_index] = k;
		interesting[ACTRNgramModel.negD_index] = negD;
		interesting[ACTRNgramModel.psp_index] = psp;
		interestingValues.offer(interesting);
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
