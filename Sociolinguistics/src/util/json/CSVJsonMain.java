package util.json;

import util.csv.JsonCSV;
import util.sys.Executor;

public class CSVJsonMain extends Executor<CSVJsonProc, JsonCSV, JsonList> {
	private final static int gbperthread = 6;
	private final static String procname = "csvjson";
	public CSVJsonMain() {
		super(procname, gbperthread, CSVJsonProc.class,JsonCSV.class, JsonList.class);
	}
	public static void main(String[] args) {
		CSVJsonMain csv = new CSVJsonMain();
		csv.initializeFromCmdLine(args);
		csv.run();
	}
}
