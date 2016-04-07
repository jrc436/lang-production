package edu.psu.acs.lang.outputreader;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import edu.psu.acs.lang.PathConsts;
import edu.psu.acs.lang.production.RetrieveHome;
import ngrams.StandardNgramModel;

public class GarbleScorer {
	public static void main(String[] args) {
		List<String> output = null;
		StandardNgramModel sng = null;
		try {
			output = Files.readAllLines(Paths.get(PathConsts.outputPath));
			sng = new StandardNgramModel(2, PathConsts.lmPath);
			
		} catch (IOException e) {
			System.err.println("A path is invalid");
			System.exit(1);
		}
		//we need to break output up into goal pursuing chunks, for now we'll assume each file is individually
		int index = 0;
		double runningAvg = 0.0;
		int totalSentences = 0;
		while (index < output.size()) {
			GarbleFragments gf = new GarbleFragments();
			while (index < output.size()) {
				gf.addGarble(GarbleFragments.getGarbleFromOutputLine(output.get(index)));
				index++;
				if (output.get(index).equals(RetrieveHome.sentenceDelimiter)) {
					totalSentences++;
					break;
				}
			}
			List<String> fragments = gf.getFragments();
			double avgScore = 0.0;
			for (String fragment : fragments) {
				avgScore += sng.easyScore(fragment);
			}
			avgScore /= fragments.size();
			runningAvg += avgScore;
		}
		System.out.println(runningAvg/totalSentences);
	}
}
