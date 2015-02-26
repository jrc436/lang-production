package opennlp.ccg;


//forgot to commit a simple file and don't want to cause git collisions

public class Client2 {
	private static final String[] filePaths = new String[] {
		"../../../ap-largefiles/data/swbd/swbd-rawtext-sample.txt",
	};
	public static final String trainingPath = "../../../ap-largefiles/data/wsj/wsj-rawtext.txt";
	
	
	private static final String grammar = "../config/grammar/grammar.xml";
	public static void main(String[] args) throws Exception {
		Realize r = new Realize();
		Parse p = new Parse();
		
		for (String in : filePaths) {
			String out1 = in.split(".txt")[0] + "-parsed.txt";
			String out2 = in.split(".txt")[0] + "-out.txt";
			p.parseMain(grammar, in, out1);
			//r.realizeMain(trainingPath, grammar, out1, out2);
		}
	}
	
}
