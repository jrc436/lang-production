package edu.psu.acs.lang.declarative;

public enum CCGBaseTypeEnum {
	S,
	NP,
	PP,
	VBP,
	N,
	Comma,
	IN, //I'm not sure if this is an actual type... but one of them has it.
	ININ, //I'm not sure if this is real, but it's on there.
	Colon,
	conj, //this refers to conjunctions (e.g. AND)
	Period;
	public static CCGBaseTypeEnum value(String s) {
		if (s.equals(".")) {
			return CCGBaseTypeEnum.Period;
		}
		else if (s.equals(",")) {
			return CCGBaseTypeEnum.Comma;
		}
		else if (s.equals(":")) {
			return CCGBaseTypeEnum.Colon;
		}
		else if (s.equals("IN^IN")) {
			return CCGBaseTypeEnum.ININ;
		}
		return valueOf(s);
	}
	public String toString() {
		switch (this) {
		case Colon:
			return ":";
		case Comma:
			return ",";
		case Period:
			return ".";
		case ININ:
			return "IN^IN";
		default:
			return super.toString();
		}
	}
}
