package evaluation;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

import runconfig.ScoringStrategy;
import realize.Realization;

/**
 * an evaluator loads a set of strings that have been placed in a set of files under a specific folder.
 * The only real requirement is that the files are in "sentence per line" format 
 * @author jrc
 */
public abstract class Evaluator implements TextScorer {	
	List<Realization> realizations;
	private ScoringStrategy strat;
	public Evaluator(ScoringStrategy strat) {
		this.strat = strat;
	}
	//if for whatever reason, it's better to load from files, then do that. Generally should not be used.
	public synchronized void loadFiles(String goldDirPath, String testDirPath) throws IOException {
		realizations = new ArrayList<Realization>();
		File[] evalFiles = new File(testDirPath).listFiles();
		Arrays.sort(evalFiles);
		File[] goldFiles = new File(goldDirPath).listFiles();
		Arrays.sort(goldFiles);
		List<String> goldSentences = new ArrayList<String>();
		for (File f : goldFiles) {
			goldSentences.addAll(Files.readAllLines(f.toPath()));
		}
		int count = 0;
		for (File f : evalFiles) {
			List<String> strings = Files.readAllLines(f.toPath());
			for (String s : strings) {
				String[] comps = s.split(Realization.splitConst);
				boolean complete = comps[1].equals("COMPLETE");
				realizations.add(new Realization(comps[0], goldSentences.get(count), complete));
				count++;
			}
		}	
	}
	public void loadData(Realization[] realize) {
		realizations = new ArrayList<Realization>(Arrays.asList(realize));		
	}
	public void loadData(List<Realization> realize) {
		realizations = realize;
	}
	public Evaluation scoreAll() {
		double completeNum = 0.0;
		double totalNum = 0.0;
		double totalScore = 0.0;
		double completeScore = 0.0;
		for (int i = 0; i < realizations.size(); i++) {
			double score =  this.score(realizations.get(i).str, realizations.get(i).goal);
			totalScore += score;
			totalNum++;
			if (realizations.get(i).complete) {
				completeNum++;
				completeScore += score;
			}
		}
		double totScore = totalScore / totalNum;
		double complScore = completeScore / completeNum;
		double totstddev = 0.0;
		double complstddev = 0.0;
		for (int i = 0; i < realizations.size(); i++) {
			double score =  this.score(realizations.get(i).str, realizations.get(i).goal);
			totstddev += Math.pow((score - totScore), 2);
			if (realizations.get(i).complete) {
				complstddev += Math.pow((score - complScore), 2);
			}
		}
		totstddev /= totalNum;
		complstddev /= completeNum;
		return new Evaluation(completeNum/totalNum, totScore, complScore, totstddev, complstddev, this.strat);
	}
}
