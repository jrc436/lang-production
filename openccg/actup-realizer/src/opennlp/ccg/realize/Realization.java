package opennlp.ccg.realize;

public class Realization {
	public final String str;
	public final boolean complete;
	public final String goal;
	public static final String splitConst = "::";
	public Realization(String str, String goal, boolean complete) {
		this.str = str;
		this.complete = complete;
		this.goal = goal;
	}
	public String toString() {
		String compl = complete ? "COMPLETE" : "INCOMPLETE";
		return this.str + splitConst + compl; 
	}
}
