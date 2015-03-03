package opennlp.ccg;

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

//forgot to commit a simple file and don't want to cause git collisions

public class ClientTerminal {
	private static final String pathToLoadAll = "../../../../ap-largefiles/data/run-list";
	private static final String prefix = "../../../../ap-largefiles/data/swbd/";

	public static final String trainingPath1 = "../../../../ap-largefiles/lm/wsj.lm";
	public static final String trainingPathDir = "../../../../ap-largefiles/lm/swbd/";
	
	public static final String defaultScorer = "opennlp.ccg.ngrams.NgramScorer";
	public static final String defaultConfig = "../../config/tagger/stconfig";
	
	//parser config
	public static final int nBestListSize = 1;
	
	private static final String grammar = "../../config/grammar/grammar.xml";
	public static final boolean USEACTR = false;
	
	public static void main(String[] args) throws Exception {
		Realize r = new Realize();
		Parse p = new Parse();
		
		boolean useACTR = USEACTR;

		String addendum = useACTR ? "-actr" : "-std";
		
		List<String> filePaths = Files.readAllLines(Paths.get(pathToLoadAll), Charset.defaultCharset());
		for (String fp : filePaths) {
			String in = prefix + fp;
			String out1 = in.split(".txt")[0] + "-parsed.xml";
			String out2 = in.split(".txt")[0] + addendum;
			
			p.parseMain(grammar, in, out1, defaultScorer, defaultConfig, nBestListSize);
			r.realizeMain(useACTR, trainingPath1, grammar, out1, out2+".wsj");
			r.realizeMain(useACTR, trainingPathDir+fp.split(".")[0]+".train", grammar, out1, out2+".swm1");
			
			useACTR = !useACTR;
			out2 = in.split(".txt")[0] + addendum;
			r.realizeMain(useACTR, trainingPath1, grammar, out1, out2+".wsj");
			r.realizeMain(useACTR, trainingPathDir+fp.split(".")[0]+".train", grammar, out1, out2+".swm1");
		}
	}
}
