package worddata;

import util.json.JsonList;
import util.sys.Executor;
import util.wordmap.WordMap;

public class WordDataMain extends Executor<WordMapCreator, JsonList, WordMap> {
	private static final String name = "worddata";
	private static final int gbPerThread = 25;
	public WordDataMain() {
		super(name, gbPerThread, WordMapCreator.class, JsonList.class, WordMap.class);
	}
	public static void main(String[] args) {
		WordDataMain wdf = new WordDataMain();
		wdf.initializeFromCmdLine(args);
		wdf.run();	
	}
	
}
