package subredditanalysis.users;

import util.csv.SConfusionCSV;
import util.listdata.UserList;
import util.sys.Executor;

public class SubredditAlignmentExecutor extends Executor<SubredditAlignmentProcessor, UserList, SConfusionCSV> {
	private static final String name = "subalignment";
	private static final int gbPerThread = 40;
	
	public SubredditAlignmentExecutor() {
		super(name, gbPerThread, SubredditAlignmentProcessor.class, UserList.class, SConfusionCSV.class);
	}

	public static void main(String args[]) {
		SubredditAlignmentExecutor fm = new SubredditAlignmentExecutor();
		fm.initializeFromCmdLine(args);
		fm.run();
	}
}
