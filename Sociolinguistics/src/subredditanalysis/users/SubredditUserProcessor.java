package subredditanalysis.users;

import util.data.Comment;
import util.json.JsonLayer;
import util.json.JsonList;
import util.json.JsonReadable;
import util.listdata.UserList;

public class SubredditUserProcessor extends JsonLayer<UserList> {
	public SubredditUserProcessor(String inpDir, String outDir, String[] cf) {
		super(inpDir, outDir, new UserList(), cf[0]);
	}
	public SubredditUserProcessor() {
		super();
	}

	@Override
	public void map(JsonList newData, UserList threadAggregate) {
		for (JsonReadable jr : newData) {
			Comment c = getAsComment(jr);
			String sr = c.getField("subreddit");
			threadAggregate.addComment(sr, c);
		}
	}
	//keywordlist provides the keywords
	@Override
	public boolean hasNArgs() {
		return false;
	}
	//needs the comment format
	@Override
	public int getNumFixedArgs() {
		return 0;
	}
	@Override
	public String getConstructionErrorMsg() {
		return super.getConstructionErrorMsg()+"; the comment format should be specified";
	}
	@Override
	public void reduce(UserList threadAggregate) {
		synchronized(processAggregate) {
			for (String key : threadAggregate.keySet()) {
				if (processAggregate.containsKey(key)) {
					processAggregate.get(key).addAll(threadAggregate.get(key));
				}
				else {
					processAggregate.put(key, threadAggregate.getCopyCollection(key));
				}
			}
		}
	}

}
