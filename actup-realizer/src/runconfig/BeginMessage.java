package runconfig;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

public class BeginMessage implements Message {
	private final IOSettings io;
	private final List<Integer> allInputs;
	private final String expName;
	public BeginMessage(IOSettings io, Set<Integer> set, String expName) {
		this.io = io;
		this.allInputs = new ArrayList<Integer>(set);
		Collections.sort(this.allInputs);
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
