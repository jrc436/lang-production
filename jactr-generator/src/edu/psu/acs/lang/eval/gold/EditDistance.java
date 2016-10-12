package edu.psu.acs.lang.eval.gold;

import java.util.List;

import edu.psu.acs.lang.eval.data.garblegold.GarbleGold;
import edu.psu.acs.lang.output.Evaluator;

public class EditDistance implements Evaluator<GarbleGold> {	
	public static double slot(String realization, String goal) {
		String[] strm1 = realization.split(" ");
		String[] strm2 = goal.split(" ");
		int dist = 0;
		for (int i = 0; i < strm2.length; i++) {
			if (strm1.length <= i || !strm1[i].equals(strm2[i])) {
				dist++;
			}
		}
		return dist;
	}

	public static double leven(String realization, String goal) {
		String[] strm1 = realization.split(" ");
		String[] strm2 = goal.split(" ");
		int[][] distance = new int[strm1.length + 1][strm2.length + 1];

		for (int i = 0; i <= strm1.length; i++) {
			distance[i][0] = i;
		}
		for (int j = 1; j <= strm2.length; j++) {
			distance[0][j] = j;
		}
		for (int i = 1; i <= strm1.length; i++) {
			for (int j = 1; j <= strm2.length; j++) {
				int term1 = distance[i - 1][j] + 1;
				int term2 = distance[i][j - 1] + 1;
				int term3 = distance[i - 1][j - 1];
				term3 += strm1[i - 1].equals(strm2[j - 1]) ? 0 : 1;
				distance[i][j] = distance[i][j] = Math.min(Math.min(term1, term2), term3);
			}
		}
		return ((double) distance[strm1.length][strm2.length]) / ((double) strm1.length);
	}

	@Override
	public String evalName() {
		return "leven,slot";
	}

	@Override
	public String evaluate(GarbleGold data) {
		double avg = 0.0;
		List<String> frags = data.getGarble().getFragments();
		for (String frag : frags) {
			avg += leven(data.getGold(), frag);
		}
		avg /= frags.size();
		double slotavg = 0.0;
		for (String frag : frags) {
			slotavg += slot(data.getGold(), frag);
		}
		slotavg /= frags.size();
		return String.format("%.3f,%.3f", avg, slotavg);
	}
}
