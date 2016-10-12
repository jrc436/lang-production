package edu.psu.acs.lang.eval.unary.mem;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.Evaluator;

public class MaxMemory implements Evaluator<GarbleGold> {

	@Override
	public String evalName() {
		return "MaxMemory";
	}

	@Override
	public String evaluate(GarbleGold data) {
		return ""+data.getGarble().getMaxMem();
	}
	
}
