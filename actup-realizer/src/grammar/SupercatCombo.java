package grammar;


//a supercat holder that doesn't count the rule
public class SupercatCombo {

	//going to point out that a lot of this seems pretty stupid. Not entirely clear why identity hashing strategies are used on strings...
	//TODO: make this less stupid
	private final String supercat;
	private final String supercat2;
	
	public SupercatCombo(String supercat) {
		this.supercat = supercat;
		this.supercat2 = null;
	}
	public SupercatCombo(String supercat, String supercat2) {
		this.supercat = supercat;
		this.supercat2 = supercat2;
	}
	
//	// supercat hashcode, excluding rule
	public int hashCode() {
		return 31*System.identityHashCode(supercat) + System.identityHashCode(supercat2);
	}
	// supercat equals
	public boolean equals(Object obj) {
		if (obj.getClass() != this.getClass()) {
			return false;
		}
		SupercatCombo combo = (SupercatCombo) obj;
		//necessary since we switched to .equals
		if ((supercat == null && combo.supercat != null) || (supercat2 == null && combo.supercat2 != null)) { return false; }
		if (supercat == null && supercat2 == null && combo.supercat == null && combo.supercat2 == null) { return true; }
		return (supercat.equals(combo.supercat) && supercat2 == null && combo.supercat2 == null) || (supercat.equals(combo.supercat) && supercat2.equals(combo.supercat2));
	}
}
