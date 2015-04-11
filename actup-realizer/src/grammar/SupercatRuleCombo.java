package grammar;

// supercat-rule combos, to support filtering on observed ones
public class SupercatRuleCombo {
	// NB: strings must be interned
	private String supercat; 
	private String supercat2;
	protected String rule;
	// unary rule constructor
	public SupercatRuleCombo(String supercat, String rule) {
		setCombo(supercat.intern(), (rule != null) ? rule.intern() : null);
	}
	// binary rule constructor
	public SupercatRuleCombo(String supercat, String supercat2, String rule) {
		setCombo(supercat.intern(), supercat2.intern(), (rule != null) ? rule.intern() : null);
	}
	// setters
	// NB: assume interned strings!
	public void setCombo(String supercat, String rule) {
		this.supercat = supercat; this.supercat2 = null; this.rule = rule;
	}
	public void setCombo(String supercat, String supercat2, String rule) {
		this.supercat = supercat; this.supercat2 = supercat2; this.rule = rule;
	}
	// hashcode
	public int hashCode() {
		return 31*System.identityHashCode(supercat) + 17*System.identityHashCode(rule) + System.identityHashCode(supercat2);
	}
	// equals
	public boolean equals(Object obj) {
		if (!(obj instanceof SupercatRuleCombo)) return false;
		SupercatRuleCombo combo = (SupercatRuleCombo) obj;
		return supercat == combo.supercat && supercat2 == combo.supercat2 && rule == combo.rule;
	}
	public SupercatCombo getSupercat() {
		return new SupercatCombo(supercat, supercat2);
	}

	// toString
	public String toString() {
		StringBuffer sb = new StringBuffer(supercat);
		if (supercat2 != null) sb.append(' ').append(supercat2);
		sb.append(' ').append(rule);
		return sb.toString();
	}
}
