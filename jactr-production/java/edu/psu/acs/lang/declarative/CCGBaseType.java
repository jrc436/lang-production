package edu.psu.acs.lang.declarative;

public class CCGBaseType extends CCGType {
	private CCGTypeModifier modifier; //base case, can be null (always null in recursive)
	private CCGBaseTypeEnum type; //base case (always null in recursive)
	private CCGBaseType(CCGBaseTypeEnum type, CCGTypeModifier mod) {
		super();
		this.type = type;
		this.modifier = mod;
	}
	public CCGBaseType(CCGBaseType copy) {
		this.modifier = copy.modifier;
		this.type = copy.type;
	}
	public static CCGBaseType makeType(CCGBaseTypeEnum type, CCGTypeModifier mod) {
		CCGBaseType ct = new CCGBaseType(type, mod);
		ct.addSlot(CCGTypeSlot.FullType, ct);
		return ct;
	}
	public String toString() {
		String modAdd = "";
//		if (modifier != null) {
//			modAdd = "["+modifier.toString()+"]";
//		}
		return type.toString()+modAdd;
	}
//	public CCGTypeModifier getModifier() {
//		return modifier;
//	}
	public CCGBaseTypeEnum getTypeEnum() {
		return type;
	}
	@Override
	public CCGType getRight() {
		return null;
	}
	@Override
	public CCGType getLeft() {
		return null;
	}
	@Override
	public CCGOperator getCombo() {
		return null;
	}
	@Override
	public boolean isConjable() {
		return modifier == CCGTypeModifier.conj;
	}
//	protected void purifyConj() {
//		makeUnconjable();
//	}
//	@Override
//	protected void makeConjable() {
//		modifier = CCGTypeModifier.conj;
//	}
//	@Override
//	protected void makeUnconjable() {
//		if (modifier == CCGTypeModifier.conj) {
//			modifier = null;
//		}
//	}
}
