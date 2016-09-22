package worddata;

import util.json.JsonLayer;
import util.json.JsonList;
import util.json.JsonReadable;
import util.wordmap.WordMap;

public class WordMapCreator extends JsonLayer<WordMap> {
	public WordMapCreator(String inpDir, String outDir, String[] funArgs, String[] clses) {
		super(inpDir, outDir, new WordMap(clses), funArgs[0]);
	}
	public WordMapCreator() {
		super();
	}
	@Override
	public void reduce(WordMap data) {
		synchronized(processAggregate) {
			processAggregate.combine(data);		
		}
	}
	@Override
	public void map(JsonList dataIn, WordMap workerAgg) {
		for (JsonReadable j : dataIn) {
			workerAgg.addCommentText(getAsComment(j));
		}	
	}
}
