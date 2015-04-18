package optimization;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;
import evaluation.Evaluation;

//an individual run data struct
public class RunData {
	private final VariableSet varVals;
	private final Evaluation eval;
	private final String runName;
	public RunData(VariableSet vars) {
		this(vars, null, null);
	}
	public RunData(VariableSet vars, Evaluation eval, String runName) {
		this.varVals = new VariableSet(vars);
		this.eval = eval;
		this.runName = runName;
	}
	public boolean improvement(RunData other) {
		if (this.eval == null) {
			return true;
		}
		else if (other.eval == null) {
			return false;
		}
		return this.eval.getScore() > other.eval.getScore();
	}
	protected Evaluation getEval() {
		return this.eval;
	}
	public String toString() {
		if (eval == null || runName == null) {
			throw new NotImplementedException();
		}
		String retval = "";
		retval += (runName + " :: ");
		retval += eval.toString();
		retval += varVals.toString();
		return retval;
	}
	//the evaluation and runName are just for purposes of logging and are optional arguments.
	public int hashCode() {
		return varVals.hashCode();
	}
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) { return false; }
		RunData other = (RunData) o;
		return varVals.equals(other.varVals);
	}
}
