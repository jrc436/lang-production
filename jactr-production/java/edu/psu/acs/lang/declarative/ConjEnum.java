package edu.psu.acs.lang.declarative;

public enum ConjEnum {
	Conjable,
	Nonconjable;
	
	public static ConjEnum value(boolean conjable) {
		return conjable ? ConjEnum.Conjable : ConjEnum.Nonconjable;
	}
}