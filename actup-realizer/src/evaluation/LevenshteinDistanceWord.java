package evaluation;

import runconfig.ScoringStrategy;


public class LevenshteinDistanceWord extends Evaluator {
	public LevenshteinDistanceWord(ScoringStrategy strat) {
		super(strat);
	}

	public double score(String realization, String goal) {
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
}
