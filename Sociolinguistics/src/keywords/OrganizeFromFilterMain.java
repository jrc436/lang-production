package keywords;

import util.json.JsonList;
import util.listdata.KeywordList;
import util.sys.Executor;

public class OrganizeFromFilterMain extends Executor<KeywordFilterInterface, JsonList, KeywordList> {
	private static final String name = "fkeywords";
	private static final int gbPerThread = 40;
	public OrganizeFromFilterMain() {
		super(name, gbPerThread, KeywordFilterInterface.class, JsonList.class, KeywordList.class);
	}

	public static void main(String args[]) {
		OrganizeFromFilterMain fm = new OrganizeFromFilterMain();
		fm.initializeFromCmdLine(args);
		fm.run();
	}
}
