package edu.psu.acs.lang.output.eval;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import edu.psu.acs.lang.RunConsts;
import edu.psu.acs.lang.hook.UtilityInitialization;
import edu.psu.acs.lang.output.OutputReader;
import edu.psu.acs.lang.output.OutputSentence;

public class GarbleScorer {
	@SuppressWarnings("unused")
	private static void scoreUtilityInit(UtilityInitialization ui, Evaluator sng) throws IOException {
		FileWriter fw = new FileWriter(RunConsts.getDistributionFilePath(ui.toString()).toFile());
		List<String> fragLines = Files.readAllLines(RunConsts.getOutSentencesFilePath(ui));
		System.out.println("Scoring: "+ui.toString());
		scoreUtility(fragLines, sng, fw);
	}
	private static void scoreUtility(List<String> fragLines, Evaluator sng, FileWriter fw) throws IOException {
		int totalSentences = 0;
		double runningAvg = 0.0;
		for (String fragLine : fragLines) {
			String[] fragments = fragLine.split(RunConsts.fragDelim);
			double avgScore = 0.0;
			for (String fragment : fragments) {
				double score = score(fragment, sng);
				avgScore += score;
			}
			avgScore /= ((double)fragments.length);
			runningAvg += avgScore;
			fw.write(avgScore + System.getProperty("line.separator"));
			totalSentences++;
		}
		double overallAvg = runningAvg / ((double)totalSentences);
		System.out.println("Total Sentences"+":"+totalSentences);
		fw.write("TOTAL:" + overallAvg);
		System.out.println("TOTAL:" + overallAvg);
		fw.close();
	}
	private static double score(String line, Evaluator sng) {
		return sng.score("<s> "+line+" </s>");
	}
	private static List<String> getControlLines() throws IOException {
		List<String> lines = new ArrayList<String>();
		for (File f : RunConsts.getInputFilesList()) {
			lines.addAll(Files.readAllLines(f.toPath()));
		}
		return lines;
	}
	private static Integer[] makeTable(LinkedList<Double> vals, LinkedList<Double> cutoffs) {
		Collections.sort(cutoffs);
		Collections.sort(vals);
		Integer[] binCounts = new Integer[cutoffs.size()];
		int counter = 0;
		while (!cutoffs.isEmpty()) {
			//there are still bins remaining potentially... but they are all zero
			if (vals.isEmpty()) {
				for (int i = counter; i < binCounts.length; i++) {
					binCounts[i] = 0;
				}
				break;
			}
			
			double cutoff = cutoffs.remove(0);
			int binCount = 0;
			while (vals.get(0) < cutoff) {
				vals.remove(0);
				binCount++;
				//there are still bins remaining potentially... but they are all zero
				if (vals.isEmpty()) {
					for (int i = counter+1; i < binCounts.length; i++) {
						binCounts[i] = 0;
					}
					break;
				}
			}
			binCounts[counter] = binCount;
			binCount = 0;
			counter++;
		}
		return binCounts;
	}
	private static void getExtremes(Path distFile) throws IOException {
		double max = 0;
		double min = Double.MAX_VALUE;
		for (String line : Files.readAllLines(distFile)) {
			double d = Double.parseDouble(line);
			if (d > max) {
				max = d;
			}
			else if (d < min) {
				min = d;
			}
		}
		System.out.println("file:"+distFile.getFileName()+"; min: "+min+"; max: "+max);
	}
	private static ArrayList<Double> getDoubles(Path distFile) throws IOException {
		ArrayList<Double> doubs = new ArrayList<Double>();
		for (String line : Files.readAllLines(distFile)) {
			doubs.add(Double.parseDouble(line));
		}
		return doubs;
	}
	private static LinkedList<Double> sample(ArrayList<Double> from, int num) {
		LinkedList<Double> outList = new LinkedList<Double>();
		Random r = new Random();
		while (num > 0) {
			if (from.isEmpty()) {
				System.err.println("Sample from list is not large enough for this.");
			}
			int ind = r.nextInt(from.size());
			outList.add(from.get(ind));
			from.remove(ind);
			num--;
		}
		return outList;
	}
	private static LinkedList<Double> getBins() {
		LinkedList<Double> bins = new LinkedList<Double>();
		double currentCutoff = 0.1;
		for (int i = 0; i < 63; i++) {
			bins.add(currentCutoff);
			currentCutoff *= 0.1;
		}
		return bins;
	}
	private static double[] perturbTable(Integer[] table) {
		double[] newTable = new double[table.length];
		for (int i = 0; i < table.length; i++) {
			newTable[i] = table[i]+0.1;
		}
		return newTable;
	}

	public static void main(String[] args) throws IOException {
		//NgramScorer sng = new NgramScorer(RunConsts.lmPath, 5);
		//SRILMScorer sng = new SRILMScorer(RunConsts.srilmNgram, RunConsts.lmPath);
		int sampleNum = 600;
		Integer[] controlbins = makeTable(sample(getDoubles(RunConsts.getDistributionFilePath("control")), sampleNum), getBins());
		double[] perturbedControl = perturbTable(controlbins);
		for (UtilityInitialization ui : UtilityInitialization.values()) {
			double chisq = 0.0;
			Integer[] bins = makeTable(sample(getDoubles(RunConsts.getDistributionFilePath(ui.toString())), sampleNum), getBins());
			double[] perturbBins = perturbTable(bins);
			for (int i = 0; i < perturbBins.length; i++) {
				chisq += (Math.pow(perturbBins[i] - perturbedControl[i], 2) / perturbedControl[i]);
			}
			System.out.println(ui.toString()+" : "+chisq);
			//writeSentences(ui);
		//List<String> controlLines = new ArrayList<String>();
		//FileWriter fw = new FileWriter(RunConsts.getDistributionFilePath("control").toFile());
		//scoreUtility(getControlLines(), sng, fw);
		
		//scoreUtility(ui, sng);
			//getExtremes(RunConsts.getDistributionFilePath(ui.toString()));
		}
		double chisq = 0.0;
		Integer[] bins = makeTable(sample(getDoubles(RunConsts.getDistributionFilePath("control")), sampleNum), getBins());
		double[] perturbBins = perturbTable(bins);
		for (int i = 0; i < perturbBins.length; i++) {
			chisq += (Math.pow(perturbBins[i] - perturbedControl[i], 2) / perturbedControl[i]);
		}
		System.out.println("control"+" : "+chisq);
		
		//10^-63, 10^-1, so 63 bins
	}
}
