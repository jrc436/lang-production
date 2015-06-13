package runconfig;

import java.util.List;

public class BeginMessage implements Message {
	private final IOSettings io;
	private final List<Integer> allInputs;
	private final String expName;
	public BeginMessage(IOSettings io, List<Integer> set, String expName) {
		this.io = io;
		this.allInputs = set;
		this.expName = expName;
	}
	public String print() {
		String retval = io.toString() + "\n";
		for (Integer i : allInputs) {
			retval+= i+",";
		}
		return retval;
	}
	public String getExpName() {
		return expName;
	}
}
