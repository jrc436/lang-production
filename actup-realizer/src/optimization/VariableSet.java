package optimization;

public class VariableSet {
	private Variable[] vars;
	private int currentIndex;
	private final int initIndex;
	
	private boolean lastSuperIterImproved;
	
	public VariableSet(Variable[] vars, int initIndex) {
		this.vars = vars;
		this.currentIndex = initIndex;
		this.initIndex = initIndex;
		this.lastSuperIterImproved = true;
	}
	public void acknowledgeImprovement() {
		this.lastSuperIterImproved = true;
	}
	public boolean step(boolean lastStepGood) {
		boolean toReturn = false;
		if (vars.length != 0) {
			try {
				toReturn = vars[currentIndex].step(lastStepGood);
			}
			catch (Exception e) {
				e.printStackTrace();
				System.err.println("Inconsistent settlement!!");
				System.exit(1);
			}
		}
		return toReturn; 
	}
	protected Variable[] getVarArray() {
		return this.vars;
	}
	public double[] getDoubleArray() {
		double[] d = new double[vars.length];
		for (int i = 0; i < vars.length; i++) {
			d[i] = vars[i].getCurrentValue();
		}
		return d;
	}
	
	//return true if we've successfully incremented the index.
	//return false if the loop should break.
	protected boolean updateIndex(boolean curVarImproving) {
		if (curVarImproving) {
			return true; //don't need to update index!
		}
		//need to update index, we also might need to start a new superIter!
		if (currentIndex + 1 == initIndex) {
			return startNewSuperIter();
		}
		currentIndex = currentIndex + 1 == vars.length ? 0 : currentIndex + 1;
		return true;
	}
	private boolean startNewSuperIter() {
		if (!lastSuperIterImproved) {
			return false;
		}
		for (Variable v : vars) {
			v.unsettle();
		}
		this.currentIndex = initIndex;
		this.lastSuperIterImproved = false; //need to check if there's an improvement again
		return true;
	}
	public void randomAll() {
		for (int i = 0; i < vars.length; i++) {
			vars[i].resetValueRandom(); //this will produce a value between 0 and 1, so it has to be normalized with the range...
		}
	}
	protected void forceAll(double[] vals) {
		for (int i = 0; i < vars.length; i++) {
			vars[i].forceCurrentValue(vals[i]);
		}
	}
}
