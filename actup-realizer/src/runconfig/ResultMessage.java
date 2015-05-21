package runconfig;

import optimization.RunData;

//mostly it's job is just to tell the outputter where to put the rundata
public class ResultMessage implements Message {
	private final RunData rd;
	public final String experimentName;
	public ResultMessage(RunData rd, String experimentName) {
		this.rd = rd;
		this.experimentName = experimentName;
	}
	public String print() {
		return rd.toString();
	}
	@Override
	public String getExpName() {
		return experimentName;
	}
}
