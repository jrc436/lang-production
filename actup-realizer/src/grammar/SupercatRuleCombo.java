package grammar;

// supercat-rule combos, to support filtering on observed ones
public class SupercatRuleCombo {
	private final String supercat; 
	private final String supercat2;
	private final String rule;
	// unary rule constructor
	public SupercatRuleCombo(String supercat, String rule) {
		this(supercat, null, rule);
	}
	// binary rule constructor
	public SupercatRuleCombo(String supercat, String supercat2, String rule) {
		this.supercat = supercat;
		this.supercat2 = supercat2;
		this.rule = rule;
	}
	protected boolean hasRule() {
		return rule != null;
	}

	// hashcode
	public int hashCode() {
		return 31*System.identityHashCode(supercat) + 17*System.identityHashCode(rule) + System.identityHashCode(supercat2);
	}
	// equals
	public boolean equals(Object obj) {
		if (!(obj instanceof SupercatRuleCombo)) return false;
		SupercatRuleCombo combo = (SupercatRuleCombo) obj;
		return supercat.equals(combo.supercat) && supercat2.equals(combo.supercat2) && rule.equals(combo.rule);
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
