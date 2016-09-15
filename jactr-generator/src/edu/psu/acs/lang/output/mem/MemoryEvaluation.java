package edu.psu.acs.lang.output.mem;

import java.util.List;

import edu.psu.acs.lang.RunConsts;
import edu.psu.acs.lang.hook.UtilityInitialization;
import edu.psu.acs.lang.output.OutputReader;
import edu.psu.acs.lang.output.OutputSentence;
import edu.psu.acs.lang.output.tree.GarbleTree;

public class MemoryEvaluation {
	public static void main(String[] args) {
		for (UtilityInitialization ui : UtilityInitialization.values()) {
			System.out.println(ui.toString()+":");
			double avgMemRatio = 0.0;
			List<OutputSentence> sent = OutputReader.getAllSentences(RunConsts.getAllOutputFiles(ui));
			for (OutputSentence s : sent) {
				GarbleTree gt = new GarbleTree(s);
				avgMemRatio += gt.getMaxMem();
			}
			avgMemRatio /= sent.size();
			System.out.println(avgMemRatio);
		}
	}
}
