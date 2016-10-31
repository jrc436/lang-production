package edu.psu.acs.lang.output.eval.unary.mem;

import edu.psu.acs.lang.output.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.eval.core.Evaluator;

public class MemRatio implements Evaluator<GarbleGold> {

		@Override
		public String evalName() {
			return "MemoryRatio";
		}

		@Override
		public String evaluate(GarbleGold data) {
			return ""+data.getGarble().getMemRatio();
		}
		
	

}
