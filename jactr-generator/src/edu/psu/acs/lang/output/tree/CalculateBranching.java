package edu.psu.acs.lang.output.tree;

import java.util.List;

import edu.psu.acs.lang.hook.UtilityInitialization;
import edu.psu.acs.lang.output.OutputReader;
import edu.psu.acs.lang.output.OutputSentence;

public class CalculateBranching {
	public static void main(String[] args) {
		for (UtilityInitialization ui : UtilityInitialization.values()) {
			System.out.println(ui.toString()+":");
			double avg = 0.0;
			List<OutputSentence> sent = OutputReader.getAllSentences(ui);
			for (OutputSentence s : sent) {
				GarbleTree gt = new GarbleTree(s);
				avg += gt.getAverageBranchFactor();
			}
			avg /= sent.size();
			System.out.println(avg);
		}
	}
}
