package synsem;


public class SignSurfaceWords {
	private final Sign s;
	public SignSurfaceWords(Sign s) {
		this.s = s;
	}
	public int hashCode() {
		return s.surfaceWordHashCode();
    }
	public boolean equals(Object o1) {
		return s.surfaceWordEquals(o1);
	}  
}
