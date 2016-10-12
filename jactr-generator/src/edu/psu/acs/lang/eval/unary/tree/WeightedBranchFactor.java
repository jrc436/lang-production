package edu.psu.acs.lang.eval.unary.tree;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.Evaluator;

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
