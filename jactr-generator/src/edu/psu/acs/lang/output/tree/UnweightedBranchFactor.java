package edu.psu.acs.lang.output.tree;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.Evaluator;

public class UnweightedBranchFactor implements Evaluator<GarbleGold> {
	
	@Override
	public String evalName() {
		return "unweightedrbf";
	}

	@Override
	public String evaluate(GarbleGold data) {
		return String.format("%.2d", data.getGarble().computeUnweightedBranchFactor());
	}
}
