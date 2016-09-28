package edu.psu.acs.lang.output.mem;

import edu.psu.acs.lang.hook.UtilityInitialization;

public class MemoryEvaluation {
	public static void main(String[] args) {
		for (UtilityInitialization ui : UtilityInitialization.values()) {
			System.out.println(ui.toString()+":");
			double avgMemRatio = 0.0;
//			List<OutputSentence> sent = OutputReader.getAllSentences(EvaluationConsts.getAllOutputFiles(ui));
//			for (OutputSentence s : sent) {
//				GarbleTree gt = new GarbleTree(s);
//				avgMemRatio += gt.getMaxMem();
//			}
//			avgMemRatio /= sent.size();
//			System.out.println(avgMemRatio);
		}
	}
}
