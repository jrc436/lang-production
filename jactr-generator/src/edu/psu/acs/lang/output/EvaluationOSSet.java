package edu.psu.acs.lang.output;

import java.util.HashSet;
import java.util.Set;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGold;
import util.sys.DataType;

public class EvaluationOSSet extends EvaluationSet<GarbleGold> {

	/**
	 * 
	 */
	public EvaluationOSSet(Set<Evaluator<GarbleGold>> set) {
		super(set);
	}
	public EvaluationOSSet() {
		super();
	}
	public EvaluationOSSet(EvaluationOSSet other) {
		super(other);
	}
	@Override
	public void add(GarbleGold newElement) {
		if (!newElement.getGarble().isEmpty()) {
			super.add(newElement);
		}
	}
	@SuppressWarnings("unchecked")
	public static EvaluationOSSet makeSet(String[] clses) {
		Set<Evaluator<GarbleGold>> set = new HashSet<Evaluator<GarbleGold>>();
		for (String cls : clses) {
			try {
				set.add((Evaluator<GarbleGold>) Class.forName(cls).newInstance());
			} catch (InstantiationException | IllegalAccessException | ClassNotFoundException e) {
				e.printStackTrace();
				System.err.println("Class:" +cls+" is not a fully qualified GarbleGold Evaluator");
				System.exit(1);
			}
		}
		return new EvaluationOSSet(set);
	}
	private static final long serialVersionUID = 7409143288823735853L;
	@Override
	public DataType deepCopy() {
		return new EvaluationOSSet(this);
	}

}
