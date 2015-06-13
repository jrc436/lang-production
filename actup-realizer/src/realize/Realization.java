package realize;

public class Realization {
	public final String str;
	public final boolean complete;
	public final String goal;
	public static final String splitConst = "::";
	private final double bestDiff;
	public Realization(String str, String goal, boolean complete, double bestDiff) {
		this.str = str;
		this.complete = complete;
		this.goal = goal;
		this.bestDiff = bestDiff;
	}
	public String toString() {
		String compl = complete ? "COMPLETE" : "INCOMPLETE";
		return this.str + splitConst + compl+splitConst+bestDiff; 
	}
}
