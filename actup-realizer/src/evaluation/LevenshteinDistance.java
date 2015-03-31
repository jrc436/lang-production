package evaluation;

import runconfig.ScoringStrategy;




//taken from wikibooks
public class LevenshteinDistance extends Evaluator {                                                        
	public LevenshteinDistance(ScoringStrategy strat) {
		super(strat);
	}

	public double score(String str1,String str2) {
		int[][] distance = new int[str1.length() + 1][str2.length() + 1];        
 
		for (int i = 0; i <= str1.length(); i++) {       
			distance[i][0] = i;                   
		}
        for (int j = 1; j <= str2.length(); j++) {
        	distance[0][j] = j;                                                  
        }
        for (int i = 1; i <= str1.length(); i++) {
        	for (int j = 1; j <= str2.length(); j++) {
        		int term1 = distance[i - 1][j] + 1;
        		int term2 = distance[i][j - 1] + 1;
        		int term3 = distance[i - 1][j - 1];
        		term3 += str1.charAt(i - 1) == str2.charAt(j - 1) ? 0 : 1;
        		distance[i][j] = Math.min(Math.min(term1, term2), term3);       
            }
        }
        return distance[str1.length()][str2.length()];                           
    }               
}
