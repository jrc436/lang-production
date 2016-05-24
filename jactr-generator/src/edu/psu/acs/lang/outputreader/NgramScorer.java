package edu.psu.acs.lang.outputreader;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NgramScorer implements Evaluator {
	private final List<Map<String, NgramValue>> ngrams;
	private boolean warned = false;
	public NgramScorer(String scorer, int maxOrder) throws IOException {
		ngrams = new ArrayList<Map<String, NgramValue>>();
		List<String> lines = Files.readAllLines(Paths.get(scorer));
		int curGrams = 0;
		boolean unkset = false;
		ngrams.add(new HashMap<String, NgramValue>());
		for (String line : lines) {
			if (line.trim().isEmpty()) {
				continue;
			}
			else if (line.equals("\\data\\")) {
				System.out.println("Data found");
				continue;
			}
			else if (line.equals("\\end\\")) {
				System.out.println("End of file");
				continue;
			}
			else if (line.contains("ngram")) {
				continue;
			}
			else if (line.contains("\\"+(curGrams+1)+"-grams")) {
				ngrams.add(new HashMap<String, NgramValue>());
				curGrams++;
				if (Integer.parseInt(""+line.charAt(1)) == curGrams) {
					System.out.println("Starting "+curGrams+"-grams");
				}
				else {
					System.err.println("Mismatch curGrams="+curGrams+" found="+line.charAt(1));
					System.err.println("line:"+line);
					System.exit(1);
				}
				continue;
			}
			String[] parts = line.split("\\s");
			String gramString = "";
			if (parts.length - 2 != curGrams && curGrams != maxOrder) {
				if (parts[1].equals("</s>")) {
					ngrams.get(curGrams).put(parts[1].toLowerCase(), new NgramValue(Double.parseDouble(parts[0])));
					continue;
				}
				else if (parts[1].equals("<unk>")) {
					ngrams.get(0).put("<unk>", new NgramValue(Double.parseDouble(parts[0])));
					unkset = true;
					continue;
				}
				System.err.println("Expected: "+curGrams+" word units, found: "+(parts.length-2));
				System.err.println(line);
				System.exit(1);
			}
			else if (curGrams == maxOrder && parts.length - 1 != curGrams) {
				System.err.println("Expected: "+curGrams+" word units, found: "+(parts.length-1));
				System.err.println(line);
				System.exit(1);
			}
			for (int i = 1; i < parts.length - 1; i++) {
				gramString += parts[i] + " ";
			}
			if (curGrams == maxOrder) {
				gramString += parts[parts.length-1];
			}
			else {
				gramString = gramString.substring(0, gramString.length()-1); //killing last space
			}
			double bow = curGrams == maxOrder ? -99 : Double.parseDouble(parts[parts.length-1]);
			double logProb = Double.parseDouble(parts[0]);
			ngrams.get(curGrams).put(gramString.toLowerCase(), new NgramValue(logProb, bow));
		}
		if (!unkset) {
			System.err.println("No unk was set to handle missing words, please provide an lm with an unk");
			ngrams.get(0).put("<unk>", new NgramValue(-1.67));
		}
	}
	public double score(String s) {
		return Evaluator.convertToProb(logProb(s, s.split(" ").length, false));
	}
	private double average(List<Double> scores) {
		double runAvg = 0;
		for (Double d : scores) {
			runAvg += d;
		}
		return runAvg /= ((double)scores.size());
	}
	private double getUnk() {
		return ngrams.get(0).get("<unk>").logProb;
	}
	//<s> and </s> should cap strings....
	private double logProb(String s, int order, boolean backoff) {
		if (order == 0) {
			return getUnk();
		}
		int maxorder = ngrams.size()-1;
		//if maxorder < order, we're going to do an average.
		if (maxorder < order) {
			if (!warned) {
				System.err.println("Order: "+order+" not contained in this scorer");
				warned = true;
			}
			String[] parts = s.split(" ");
			List<Double> indScores = new ArrayList<Double>();
			for (int i = 0; i < parts.length-maxorder; i++) {
				String make = "";
				for (int j = i; j < i + maxorder; j++) {
					make += parts[j] + " ";
				}
				make = make.substring(0, make.length()-1);
				indScores.add(logProb(make, maxorder, false));
			}
			return average(indScores);
		}
		//if maxorder = order, and the map contains the key, we just do a retrieval
		if (ngrams.get(order).containsKey(s)) {
			return backoff ? ngrams.get(order).get(s).bow : ngrams.get(order).get(s).logProb;
		}
		else if (order == 1) {
			return getUnk();
		}
		//otherwise, either way, we have to do a backoff.
		//If maxorder > order && backoff = false, that might mean we're evaluating a 4-gram with a 5-gram model. 
		//If maxorder > order && backoff = false, we'll return [logProb(s, order, false) -(abs(unk) * (maxorder-order)]
		//if maxorder == order && backoff = false, then we have to backoff. This implies taking the product of 
		String[] parts = s.split(" ");
		return logProb(s.substring(0, s.length()-parts[parts.length-1].length()), order-1, true) + logProb(parts[parts.length-1], 1, true);
	}
	private class NgramValue {
		private final double logProb;
		private final double bow;
		private NgramValue(double logProb) {
			this.logProb = logProb;
			this.bow = -99;
		}
		private NgramValue(double logProb, double bow) {
			this.logProb = logProb;
			this.bow = bow;
		}
	}
}
