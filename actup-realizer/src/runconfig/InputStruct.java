package runconfig;

import synsem.LF;

public class InputStruct {
	private final LF lf;
	public LF getLF() {
		return lf.copy(); //lfs are not immutable.
	}
	private final String goal;
	public String getGoal() {
		return goal;
	}
	private final int fileNum;
	public int getFileNum() {
		return fileNum;
	}
	InputStruct(LF lf, String goal, int fileNum) {
		this.lf = lf;
		this.goal = goal;
		this.fileNum = fileNum;
	}
}
