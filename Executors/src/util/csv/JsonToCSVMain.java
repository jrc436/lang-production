package util.csv;

import util.json.JsonList;
import util.sys.Executor;

public class JsonToCSVMain extends Executor<JsonCSVProc, JsonList, JsonCSV> {
	private final static String procname = "jsoncsv";
	private final static int gbperthread = 5;
	public JsonToCSVMain() {
		super(procname, gbperthread, JsonCSVProc.class, JsonList.class, JsonCSV.class);
	}
	public static void main(String[] args) {
		JsonToCSVMain js = new JsonToCSVMain();
		js.initializeFromCmdLine(args);
		js.run();
	}
}
