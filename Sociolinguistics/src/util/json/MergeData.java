package util.json;

import java.io.File;

public class MergeData extends JsonLayer<JsonKeyMap> {
	
	//private final CommentFormat secondSource;
	private final JsonKeyMap smallData;
	private final String mergeKey;
	
	//arg1 - commentformat
	//arg2 - primarykey
	//arg3 - secondsource
	public MergeData(String inpDir, String outDir, String[] procArgs, String[] outArgs) {
		super(inpDir, outDir, new JsonKeyMap(outArgs[0]), procArgs[0]);
		this.mergeKey = outArgs[0];
		smallData = new JsonKeyMap(mergeKey);
		for (File f : super.makeInputFileQueue(procArgs[1])) {
			JsonList jl = JsonLayer.getReadable(f);
			JsonKeyMap jkm = new JsonKeyMap(mergeKey, jl);
			smallData.appendMap(jkm);
		}
	}

	@Override
	public void map(JsonList newData, JsonKeyMap threadAggregate) {
		JsonKeyMap jkm = new JsonKeyMap(mergeKey, newData);
		jkm.mergeMapJsons(smallData);
		threadAggregate.appendMap(jkm);
	}

	@Override
	public void reduce(JsonKeyMap threadAggregate) {
		synchronized(processAggregate) {
			this.processAggregate.appendMap(threadAggregate);
		}
	}

	@Override
	public int getNumFixedArgs() {
		return super.getNumFixedArgs() + 1;
	}

	@Override
	public String getConstructionErrorMsg() {
		return super.getConstructionErrorMsg() + "; Merge Data requires the input directory of the merging data";
	}	

}
