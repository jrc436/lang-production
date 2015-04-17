package optimization;

import java.util.Random;

public class VariableSet {
	private final Variable[] vars;
	private int currentIndex;
	private int initIndex; //can't be final because randomall should reset it.
	
	private boolean lastSuperIterImproved;
	
	//copy constructor
	public VariableSet(VariableSet deepCopy) {
		this.initIndex = deepCopy.initIndex;
		this.currentIndex = deepCopy.currentIndex;
		this.vars = new Variable[deepCopy.vars.length];
		for (int i = 0; i < vars.length; i++) {
			vars[i] = new Variable(deepCopy.vars[i].getCurrentValue(), deepCopy.vars[i].getLower(), deepCopy.vars[i].getUpper(), deepCopy.vars[i].getIncrement());
		}
		this.lastSuperIterImproved = true;
		this.startNewSuperIter();
	}
	
	public VariableSet(Variable[] vars, int initIndex) {
		this.vars = vars;
		this.currentIndex = initIndex;
		this.initIndex = initIndex;
		this.lastSuperIterImproved = false; //otherwise, we're guaranteed two super iters, don't necessarily need to be
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
		boolean retval = false;
		if (currentIndex + 1 == initIndex || (initIndex == 0 && currentIndex == vars.length-1)) {
			retval = startNewSuperIter();
		}
		else {
			//since we're on the same superiter here, we shouldn't need to unsettle anything or declare it explicitly the first run.
			//since it hasn't yet been touched, it's implicitly the first run and unsettled
			currentIndex = currentIndex + 1 == vars.length ? 0 : currentIndex + 1;
			retval = true;
		}
		//now that we've definitely updated the index, we also want to do a step, because otherwise we'll do the same run twice.
		if (retval) { this.step(false); } //doesn't really matter what the input is, since firstRun should always be true here.
		return retval;
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
		this.currentIndex = vars.length == 0 ? 0 : new Random().nextInt(vars.length);
		this.initIndex = currentIndex;
		this.lastSuperIterImproved = true;
		this.startNewSuperIter();
	}
	protected void forceAll(double[] vals) {
		for (int i = 0; i < vars.length; i++) {
			vars[i].forceCurrentValue(vals[i]);
		}
	}
}
