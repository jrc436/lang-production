package edu.psu.acs.lang.declarative;

public enum CCGOperator implements SlotValue {
	Slash,
	Backslash;
	public String toString() {
		switch(this) {
			case Slash:
				return "/";
			case Backslash:
				return "\\";
			}
		return null;
	}
	public CCGOperator value(String s) {
		if (s.equals("/")) {
			return CCGOperator.Slash;
		}
		else if (s.equals("\\")) {
			return CCGOperator.Backslash;
		}
		return CCGOperator.valueOf(s);
	}
}
