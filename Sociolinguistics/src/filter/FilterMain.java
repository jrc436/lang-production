package filter;

import util.sys.Executor;
import util.wordmap.WordMap;

public class FilterMain extends Executor<WMFilterProcessor, WordMap, WordMap> {
	private static final String name = "filter";
	private static final int gbPerThread = 4;
	public FilterMain() {
		super(name, gbPerThread, WMFilterProcessor.class, WordMap.class, WordMap.class);
	}

	public static void main(String args[]) {
		FilterMain fm = new FilterMain();
		fm.initializeFromCmdLine(args);
		fm.run();
	}
}
