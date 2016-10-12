package edu.psu.acs.lang.output;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import util.sys.DataType;
import util.sys.FileWritable;

public class EvaluationSet<K> extends HashMap<K, List<String>> implements DataType {
	private final Set<Evaluator<K>> evals;
	public EvaluationSet() {
		super();
		this.evals = null;
	}
	public EvaluationSet(EvaluationSet<K> other) {
		super(other);
		this.evals = other.evals;
	}
	public EvaluationSet(Set<Evaluator<K>> evals) {
		this.evals = evals;
	}
	public void add(K newElement) {
		if (!this.containsKey(newElement)) {
			List<String> evalResults = new ArrayList<String>();
			for (Evaluator<K> eval : evals) {
				evalResults.add(eval.evaluate(newElement));
			}
			this.put(newElement, evalResults);
		}
	}
	private static final long serialVersionUID = -7281752224776663457L;

	@Override
	public int getNumFixedArgs() {
		return 0;
	}

	@Override
	public boolean hasNArgs() {
		return true;
	}

	@Override
	public String getConstructionErrorMsg() {
		return "The evaluation set requires fully qualified classnames of type evaluation.";
	}

	@Override
	public String getFileExt() {
		return ".csv";
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> evals = new ArrayList<String>();
		for (K s : this.keySet()) {
			String addval = s.toString() + ",";
			for (String c : this.get(s)) {
				addval += c + ",";
			}
			evals.add(addval.substring(0, addval.length()-1));
		}
		return evals;
	}

	@Override
	public String getHeaderLine() {
		String header = "data,";
		for (Evaluator<K> c : this.evals) {
			header += c.evalName() + ",";
		}
		return header.substring(0, header.length()-1);
	}

	@Override
	public String getFooterLine() {
		return null;
	}
	private String entryString(Map.Entry<K, List<String>> entry) {
		String ret = entry.getKey().toString() +",";
		for (String s : entry.getValue()) {
			ret += s + ",";
		}
		return ret.substring(0, ret.length()-1);
	}

	@Override
	public Iterator<String> getStringIter() {
		 return FileWritable.<Map.Entry<K, List<String>>, Set<Map.Entry<K, List<String>>>>iterBuilder(this.entrySet(), this::entryString);
	}

	@Override
	public DataType deepCopy() {
		return new EvaluationSet<K>(this);
	}

}
