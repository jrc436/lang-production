package opennlp.ccg;



//forgot to commit a simple file and don't want to cause git collisions

public class ClientEclipse {
	private static final String[] filePaths = new String[] {
		"../../../ap-largefiles/data/swbd/swbd-rawtext-sample.txt",
	};
	public static final String trainingPath = "../../../ap-largefiles/data/wsj/wsj-lm";
	
	public static final String defaultScorer = "opennlp.ccg.ngrams.NgramScorer";
	public static final String defaultConfig = "../config/tagger/stconfig";
	
	//parser config
	public static final int nBestListSize = 1;
	
	//Realizer config
	public static final int order = 3;
	
	public static final boolean USEACTR = true;
	
	private static final String grammar = "../config/grammar/grammar.xml";
	public static void main(String[] args) throws Exception {
		Realize r = new Realize();
		//Parse p = new Parse();
		boolean useACTR = USEACTR;
		if (args.length > 0) {
			if (args[1].equals("-actr")) {
				useACTR = true;
			}
			else {
				useACTR = false;
			}
		}	
	
		String addendum = USEACTR ? "-actr" : "-std";
		for (String in : filePaths) {
			String out1 = in.split(".txt")[0] + "-parsed.txt";
			String out2 = in.split(".txt")[0] + "-out"+addendum+".txt";
			//p.parseMain(grammar, in, out1, defaultScorer, defaultConfig, nBestListSize);
			r.realizeMain(useACTR, trainingPath, grammar, out1, out2);
		}
	}
	
}
