package edu.psu.acs.lang.eval.target;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.psu.acs.lang.hook.UtilityInitialization;
import edu.psu.acs.lang.output.OutputReader;
import edu.psu.acs.lang.output.OutputSentence;
import edu.psu.acs.lang.settings.EvaluationConsts;

public class Comparer {
	public static Map<UtilityInitialization, List<Double>> evaluateAll(IGoalComparer scorer) throws IOException {
		Map<UtilityInitialization, List<Double>> scores = new HashMap<UtilityInitialization, List<Double>>();
		for (UtilityInitialization ui : UtilityInitialization.values()) {
			scores.put(ui, new ArrayList<Double>());
		}
		
		for (int i = 0; i < 50; i++) {
			File target = EvaluationConsts.getFixedGoldFile(i);
			if (!target.exists())	 {
				continue;
			}
			List<String> targetSents = Files.readAllLines(target.toPath());
			for (UtilityInitialization ui : UtilityInitialization.values()) {
				List<OutputSentence> os = OutputReader.getAllSentences(EvaluationConsts.getAllOutputFiles(i, ui));
				if (os.isEmpty()) {
					continue;
				}
				List<String> sent = OutputReader.treeSentences(os);

				if (sent.size() != targetSents.size()) {
					System.err.println("For exp: "+i+" ui: "+ui.toString()+"; target="+targetSents.size()+"; realized="+sent.size());
					System.err.println("Skipping!");
					continue;
				}
				
				for (int j = 0; j < sent.size(); j++) {
					double bestDist = Integer.MAX_VALUE;
					for (String frag : OutputReader.getFragments(sent.get(j))) {
						bestDist = Math.min(bestDist, scorer.score(frag, targetSents.get(j)));
					}
					scores.get(ui).add(bestDist);
				}
			}
		}
		return scores;
	}
	public static Map<UtilityInitialization, Double> evaluateAllAvg(IGoalComparer scorer) throws IOException {
		Map<UtilityInitialization, List<Double>> scores = evaluateAll(scorer);
		Map<UtilityInitialization, Double> avg = new HashMap<UtilityInitialization, Double>();
		for (UtilityInitialization ui : UtilityInitialization.values()) {
			double score = 0.0;
			for (double d : scores.get(ui)) {
				score += d;
			}
			score /= scores.get(ui).size();
			avg.put(ui, score);
		}
		return avg;
	}
}
