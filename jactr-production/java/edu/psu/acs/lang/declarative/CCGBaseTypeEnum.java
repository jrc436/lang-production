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
	Period,
	TOP,
	conj; //this refers to conjunctions (e.g. AND)
	public static CCGBaseTypeEnum value(String s) {
		if (s.equals(".") || s.equals("PER")) {
			return CCGBaseTypeEnum.Period;
		}
		else if (s.equals(",") || s.equals("COM")) {
			return CCGBaseTypeEnum.Comma;
		}
		else if (s.equals(":") || s.equals("COL")) {
			return CCGBaseTypeEnum.Colon;
		}
		if (s.equals("IN^IN")) {
			return CCGBaseTypeEnum.ININ;
		}
		return valueOf(s);
	}
	public String toString() {
		switch (this) {
			case Colon:
				return "COL";
			case Comma:
				return "COM";
			case Period:
				return "PER";
			case ININ:
				return "IN^IN";
			default:
			return super.toString();
		}
	}
}
