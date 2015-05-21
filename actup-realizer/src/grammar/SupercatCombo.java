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
		return supercat == combo.supercat && supercat2 == combo.supercat2;
	}
}
