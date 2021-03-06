package edu.psu.acs.lang.production;

public enum SyntaxRuleType {
	ForwardApplication,
	ForwardComposition,
	BackwardApplication,
	TypeRaise, //just for parsing:
	PCT, //just for parsing
	TCR, //just for parsing
	TPC, //just for parsing
	UNK, //just for parsing
	CONJ, //this is the parsing conj
	CONJR, //this is the rule for conj right
	CONJL, //this is the rule for conj left
	CONJDR, //this is the rule for conj - double type on the right
	CONJDL, //this is the rule for conj - double type on the left
	BackwardComposition;
	public static SyntaxRuleType parseSyntaxRule(String s) {
		if (!s.contains("comb")) {
			throw new IllegalArgumentException("String s should contain a combinatory rule. It contained: "+s);
		}
		if (s.contains("B>")) {
			return SyntaxRuleType.ForwardComposition;
		}
		else if (s.contains("B<")) {
			return SyntaxRuleType.BackwardComposition;
		}
		else if (s.contains("T>") || (s.contains("T<"))) {
			return SyntaxRuleType.TypeRaise;
		}
		else if (s.contains(">")) {
			return SyntaxRuleType.ForwardApplication;
		}
		else if (s.contains("<")) {
			return SyntaxRuleType.BackwardApplication;
		}
		else if (s.contains("TCR")) {
			return SyntaxRuleType.TCR;
		}
		else if (s.contains("TPC")) {
			return SyntaxRuleType.TPC;
		}
		else if (s.contains("PCT")) {
			return SyntaxRuleType.PCT;
		}
		else if (s.contains("CONJ")) {
			return SyntaxRuleType.CONJ;
		}
		else if (s.contains("UNK")) {
			return SyntaxRuleType.UNK;
		}
		throw new IllegalArgumentException("String s should contain a combinatory rule. It contained: "+s);
	}
	public String getName() {
		return super.toString();
	}
	public boolean isTypeRaise() {
		switch (this) {	
			case TCR:
				return true;
			case TPC:
				return true;
			case TypeRaise:
				return true;
			default:
				return false;
		}
	}
	public String toString() {
		switch (this) {
			case BackwardApplication:
				return "<";
			case BackwardComposition:
				return "B<";
//			case CONJ:
//				return "CONJ";
			case CONJDL:
				return "CONJ";
			case CONJDR:
				return "CONJ";
			case CONJL:
				return "CONJ";
			case CONJR:
				return "CONJ";
			case ForwardApplication:
				return ">";
			case ForwardComposition:
				return "B>";
//			case PCT:
//				return "PCT";
//			case TCR:
//				return "TCR";
			case TypeRaise:
				return "T>";
			default:
				break;		
		}
		//System.out.println("Giving toString to type:"+super.toString());
		return super.toString();
	}
}
