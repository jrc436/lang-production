package edu.psu.acs.lang.output.eval.unary.tree;

import edu.psu.acs.lang.output.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.eval.core.Evaluator;

public class WeightedBranchFactor implements Evaluator<GarbleGold> {

	@Override
	public String evalName() {
		return "weightedrbf";
	}

	@Override
	public String evaluate(GarbleGold data) {
		return String.format("%.2d", data.getGarble().computeWeightedBranchFactor());
	}

}
