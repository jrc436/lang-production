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
			vars[i] = new Variable(deepCopy.vars[i].getName(), deepCopy.vars[i].getCurrentValue(), deepCopy.vars[i].getLower(), deepCopy.vars[i].getUpper(), deepCopy.vars[i].getIncrement());
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
		return vars.length != 0 ? vars[currentIndex].step(lastStepGood) : false; 
	}
	public int getNumVars() {
		return this.vars.length;
	}
	public double[] getDoubleArray() {
		double[] d = new double[vars.length];
		for (int i = 0; i < vars.length; i++) {
			d[i] = vars[i].getCurrentValue();
		}
		return d;
	}
	public void setWithDoubleArray(double[] newCurrentValues) {
		if (newCurrentValues.length != vars.length) {
			System.err.println("Can't set a variable set partially with this method. Please specify with the correct number of values");
			System.exit(1);
		}
		for (int i = 0; i < newCurrentValues.length; i++) {
			vars[i].forceCurrentValue(newCurrentValues[i]);
		}
	}
	//return true if we've successfully incremented the index.
	//return false if the loop should break.
	protected boolean updateIndex() {
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
	
	public int hashCode() {
		int hash = 0;
		for (Variable v : vars) {
			hash += v.hashCode();
		}
		return hash;
	}
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) { return false; }
		VariableSet vs = (VariableSet) o;
		if (vars.length != vs.vars.length) {
			return false;
		}
		boolean equals = true;
		for (int i = 0; i < vars.length; i++) {
			equals = equals && vars[i].equals(vs.vars[i]);
		}
		return equals;
	}
	public String toString() {
		String retval = "";
		for (Variable v : vars) {
			retval += v.toString() + " ";
		}
		return retval;
	}
}
