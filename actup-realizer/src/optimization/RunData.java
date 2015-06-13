package optimization;

import java.util.List;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;
import evaluation.Evaluation;

//an individual run data struct. It contains all of the information from running on all of the files with one set of variables
public class RunData {
	private final VariableSet varVals;
	private final Evaluation eval;
	private final String runName;
	private final long completionTime;
	private final List<Integer> files;
	public RunData(VariableSet vars) {
		this(null, vars, null, null, 0);
	}
	public RunData(List<Integer> file, VariableSet vars, Evaluation eval, String runName, long completionTime) {
		this.varVals = new VariableSet(vars);
		this.eval = eval;
		this.runName = runName;
		this.completionTime = completionTime;
		this.files = file;
	}
	public boolean improvement(RunData other) {
		if (this.eval == null) {
			return false;
		}
		else if (other.eval == null) {
			return true;
		}
		//low scores are good, remember! so it's an improvement if it's lower than other
		return this.eval.getScore() < other.eval.getScore();
	}
	protected Evaluation getEval() {
		return this.eval;
	}
	public String toString() {
		if (eval == null || runName == null) {
			throw new NotImplementedException();
		}
		String st = "Files used:";
		for (int f : files) {
			st += f+",";
		}
		st+="\n";
		String retval = "";
		retval += (runName + " :: ");
		retval += eval.toString()+" ";
		retval += varVals.toString();
		retval += "; "+(completionTime/1000000000);
		return st+retval;
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
