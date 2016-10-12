package edu.psu.acs.lang.output.tree;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.Evaluator;

public class BranchFactorEval implements Evaluator<GarbleGold> {
	
	@Override
	public String evalName() {
		return "urbf,wrbf";
	}

	@Override
	public String evaluate(GarbleGold data) {
		return String.format("%.3f,%.3f", data.getGarble().computeUnweightedBranchFactor(), data.getGarble().computeWeightedBranchFactor());
	}

}
