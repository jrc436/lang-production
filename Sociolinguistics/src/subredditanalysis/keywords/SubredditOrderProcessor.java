package subredditanalysis.keywords;

import java.io.File;

import util.data.Comment;
import util.data.CommentFormat;
import util.listdata.KeywordList;
import util.sys.FileProcessor;
import util.wordmap.WordMap;

public class SubredditOrderProcessor extends FileProcessor<KeywordList, WordMap> {
	private final CommentFormat cf;
	public SubredditOrderProcessor() {
		super();
		this.cf = null;
	}
	public SubredditOrderProcessor(String origin, String dest, String[] cf, String[] clses) {
		super(origin, dest, new WordMap(clses));
		this.cf = CommentFormat.fromString(cf[0]);
	}
	@Override
	public int getNumFixedArgs() {
		return 0;
	}

	@Override
	public boolean hasNArgs() {
		return false;
	}

	@Override
	public String getConstructionErrorMsg() {
		return null;
	}

	@Override
	public KeywordList getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		return KeywordList.createFromFile(f, cf);
	}

	@Override
	public void map(KeywordList newData, WordMap threadAggregate) {
		for (String key : newData.keySet()) {
			String word = key;
			for (Comment c : newData.get(key)) {
				threadAggregate.putWord(word, c);
			}
		}
	}

	@Override
	public void reduce(WordMap threadAggregate) {
		synchronized(processAggregate) {
			processAggregate.combine(threadAggregate);		
		}
	}

}
