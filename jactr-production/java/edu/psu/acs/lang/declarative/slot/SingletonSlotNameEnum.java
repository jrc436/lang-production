package edu.psu.acs.lang.declarative.slot;

public enum SingletonSlotNameEnum {
	goal;
	public String toString() {
		switch(this) {
			case goal:
				return "sentgoal";
			default:
				break;			
		}
		return null;
	}
}
