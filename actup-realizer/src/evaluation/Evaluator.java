package evaluation;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import runconfig.ScoringStrategy;
import realize.Realization;

/**
 * an evaluator loads a set of strings that have been placed in a set of files under a specific folder.
 * The only real requirement is that the files are in "sentence per line" format 
 * @author jrc
 */
public abstract class Evaluator implements TextScorer {	
	ArrayList<Realization> realizations;
	private double completeNum = 0.0;
	private double incompleteNum = 0.0;
	private ScoringStrategy strat;
	public Evaluator(ScoringStrategy strat) {
		this.strat = strat;
	}
	public void loadFiles(String goldDirPath, String testDirPath) throws IOException {
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
	public Evaluation scoreAll() {
		completeNum = 0.0;
		incompleteNum = 0.0;
		double totalScore = 0.0;
		double completeScore = 0.0;
		for (int i = 0; i < realizations.size(); i++) {
			double score =  this.score(realizations.get(i).str, realizations.get(i).goal);
			totalScore += score;
			if (realizations.get(i).complete) {
				completeNum++;
				completeScore += score;
			}
			else {
				incompleteNum++;
			}
		}
		return new Evaluation(completeNum/(completeNum+incompleteNum), totalScore/(completeNum+incompleteNum), completeScore/completeNum, this.strat);
	}
	public double getCompleteness() {
		if (incompleteNum+completeNum == 0.0) {
			return -1.0;
		}
		return completeNum / (completeNum + incompleteNum);
	}
}
