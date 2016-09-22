package subredditanalysis.keywords;

import java.io.File;
import java.util.List;

import util.csv.SConfusionCSV;
import util.sys.FileProcessor;
import util.wordmap.SubredditListCombine;
import util.wordmap.WordMap;

public class OriginDestinationProcessor extends FileProcessor<WordMap, SConfusionCSV> {
	public OriginDestinationProcessor() {
		super();
	}
	public OriginDestinationProcessor(String inpDir, String outDir) {
		super(inpDir, outDir, new SConfusionCSV(false));
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
		return "OriginDestionationProcessor needs no arguments.";
	}

	@Override
	public WordMap getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		return WordMap.createFromFile(f);
	}

	@Override
	public void map(WordMap newData, SConfusionCSV threadAggregate) {
		for (String key : newData.keySet()) {
			SubredditListCombine slc = (SubredditListCombine) newData.getBy(key, SubredditListCombine.class);
			List<String> subreddits = slc.produceOrdering();
			String origin = subreddits.remove(0);
			for (String dest : subreddits) {
				if (threadAggregate.containsKey(origin, dest)) {
					threadAggregate.put(origin, dest, threadAggregate.get(origin, dest)+1);
				}
			}
		}
	}

	@Override
	public void reduce(SConfusionCSV threadAggregate) {
		synchronized(processAggregate) {
			processAggregate.absorb(threadAggregate);
		}
	}

}
