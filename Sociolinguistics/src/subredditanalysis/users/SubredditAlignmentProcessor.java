package subredditanalysis.users;

import java.io.File;

import util.csv.SConfusionCSV;
import util.listdata.UserList;
import util.sys.FileProcessor;

public class SubredditAlignmentProcessor extends FileProcessor<UserList, SConfusionCSV>{
	public SubredditAlignmentProcessor(String inpDir, String outDir, String[] bool) {
		super(inpDir, outDir, new SConfusionCSV(bool));
	}
	public SubredditAlignmentProcessor() {
		super();
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
		return "SubredditAlignmentProcessor requires no additional arguments";
	}

	@Override
	public UserList getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		return UserList.createFromFile(f);
	}

	@Override
	public void map(UserList newData, SConfusionCSV threadAggregate) {
		for (String subreddit : newData.keySet()) {
			for (String othersubreddit : newData.keySet()) {
				for (String user : newData.get(subreddit)) {
					if (newData.get(othersubreddit).contains(user)) {
						//append to their confusion
						int updateValue = threadAggregate.containsKey(subreddit, othersubreddit) ? threadAggregate.get(subreddit, othersubreddit)+1 : 1;
						threadAggregate.put(subreddit, othersubreddit, updateValue);
					}
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
