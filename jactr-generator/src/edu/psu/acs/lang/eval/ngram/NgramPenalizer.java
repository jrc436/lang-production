//package edu.psu.acs.lang.eval.ngram;
//
//import java.io.IOException;
//import java.nio.file.Files;
//import java.nio.file.Path;
//import java.nio.file.Paths;
//import java.util.List;
//
//import edu.psu.acs.lang.output.Evaluator;
//import edu.psu.acs.lang.sentences.GarbleGold;
//import edu.psu.acs.lang.settings.ExperimentSettings;
//
//public class NgramPenalizer implements Evaluator<GarbleGold> {
//	private final static Path hack = Paths.get(ExperimentSettings.workingDir+"/experiments/v1.5/ngram-compare/ngram-compare.csv");
//	private final double[] diffs;
//	public NgramPenalizer() {
//		double[] diffs = null;
//		try {
//			List<String> lines = Files.readAllLines(hack);
//			diffs = new double[lines.size()-1];
//			for (int i = 1; i < lines.size(); i++) {
//				String[] parts = lines.get(i).split(",");
//				double gold = Double.parseDouble(parts[0]);
//				double garble = Double.parseDouble(parts[1]);
//				diffs[i-1] = Math.abs(gold - garble);
//			}
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		this.diffs = diffs;
//	}
//	@Override
//	public String evalName() {
//		return "ngram-diff";
//	}
//
//	@Override
//	public String evaluate(GarbleGold data) {
//		
//	}
//
//}
