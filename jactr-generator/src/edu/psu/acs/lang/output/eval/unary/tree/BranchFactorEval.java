package edu.psu.acs.lang.output.eval.unary.tree;

import edu.psu.acs.lang.output.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.eval.core.Evaluator;

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
