package edu.psu.acs.lang.declarative;



public class LSSlotName implements SlotName {
	private LSSlotNameEnum en;
	private String typeNum = "";
	public LSSlotName(LSSlotNameEnum en) {
		this.en = en;
		switch(en) {
			case Word:
				break;
			default:
				throw new UnsupportedOperationException();	
		}
	}
	public LSSlotName(LSSlotNameEnum en, int typeNum, int maxTypes) {
		this.en = en;
		int wordDigits = String.valueOf(maxTypes).length();
		String formatterI = "%0"+wordDigits+"d";
		String formattedI = String.format(formatterI, typeNum);
		switch(en) {
			case Conj:
				this.typeNum = formattedI;
				break;
			case Combinator:
				this.typeNum = formattedI;
				break;
			case LeftType:
				this.typeNum = formattedI;
				break;
			case RightType:
				this.typeNum = formattedI;
				break;
			case Type:
				this.typeNum = formattedI;
				break;
			default:
				throw new UnsupportedOperationException();		
		}
	}
	public String toString() {
		String def = en.toString();
		if (!typeNum.isEmpty()) {
			def += typeNum;
		}
		return def;
	}
}
