package edu.psu.acs.lang.output.mem;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.Evaluator;

public class MemoryEvaluation implements Evaluator<GarbleGold> {
	@Override
	public String evalName() {
		return "MemoryRatio,MaxMemory";
	}

	@Override
	public String evaluate(GarbleGold data) {
		return ""+data.getGarble().getMemRatio()+","+data.getGarble().getMaxMem();
	}
}
