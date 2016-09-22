package util.csv;

import util.json.JsonLayer;
import util.json.JsonList;

public class JsonCSVProc extends JsonLayer<JsonCSV> {

	public JsonCSVProc(String inpDir, String outDir, String[] commentFormat) {
		super(inpDir, outDir, new JsonCSV(), commentFormat[0]);
	}

	@Override
	public void map(JsonList newData, JsonCSV threadAggregate) {
		threadAggregate.addAll(newData);
	}

	@Override
	public void reduce(JsonCSV threadAggregate) {
		synchronized (processAggregate) {
			super.processAggregate.addAll(threadAggregate);
		}
	}

}
