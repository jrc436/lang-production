package edu.psu.acs.lang.declarative;

public class CCGBaseType extends CCGType {
	private CCGTypeModifier modifier; //base case, can be null (always null in recursive)
	private CCGBaseTypeEnum type; //base case (always null in recursive)
	CCGBaseType(CCGBaseTypeEnum type, CCGTypeModifier mod) {
		super();
		this.type = type;
		this.modifier = mod;
	}
	public String toString() {
		String modAdd = "";
		if (modifier != null) {
			modAdd = "["+modifier.toString()+"]";
		}
		return type.toString()+modAdd;
	}
}
