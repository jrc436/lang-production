package edu.psu.acs.lang.output.eval.gold;
//package edu.psu.acs.lang.eval.target;
//
//import java.io.File;
//import java.io.IOException;
//import java.nio.file.Files;
//import java.util.ArrayList;
//import java.util.List;
//
//import edu.psu.acs.lang.output.OutputReader;
//import edu.psu.acs.lang.output.OutputSentence;
//
//public class Comparer {
//	public static List<Double> evaluateAll(IGoalComparer scorer, File inpDir, File goldDir) throws IOException {
//		List<Double> scores = new ArrayList<Double>();
//		File[] files = goldDir.listFiles();
//		for (File target : files) {
//			List<String> targetSents = Files.readAllLines(target.toPath());
//			List<OutputSentence> os = OutputReader.getAllSentences(inpDir.listFiles());
//			if (os.isEmpty()) {
//				continue;
//			}
//			List<String> sent = OutputReader.treeSentences(os);
//
//			if (sent.size() != targetSents.size()) {
//				//System.err.println("Exp File: "+; target="+targetSents.size()+"; realized="+sent.size());
//				System.err.println("experiment realized: "+sent.size()+" out of "+targetSents.size()+" sentences. Evaluation is not possible.");
//				System.err.println("Skipping!");
//				continue;
//			}
//			
//			for (int j = 0; j < sent.size(); j++) {
//				double bestDist = Integer.MAX_VALUE;
//				for (String frag : OutputReader.getFragments(sent.get(j))) {
//					bestDist = Math.min(bestDist, scorer.score(frag, targetSents.get(j)));
//				}
//				scores.add(bestDist);
//			}
//		}
//		return scores;
//	}
//	public static List<Double> evaluateAllAvg(IGoalComparer scorer, File inpDir, File goldDir) throws IOException {
//		List<Double> scores = evaluateAll(scorer, inpDir, goldDir);
//		List<Double> avg = new ArrayList<Double>();
//		double score = 0.0;
//		for (double d : scores) {
//			score += d;
//		}
//		score /= scores.size();
//		avg.add(score);	
//		return avg;
//	}
//}
