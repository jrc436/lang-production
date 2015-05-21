package runconfig;

import optimization.RunData;

public class EndMessage implements Message {
	private String expName;
	private RunData bestRun;
	public EndMessage(RunData bestRun, String experimentName) {
		this.expName = experimentName;
		this.bestRun = bestRun;
	}
	public String print() {
		return "Best: "+bestRun.toString();
	}
	@Override
	public String getExpName() {
		return expName;
	}
}
