package edu.psu.acs.lang.declarative;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;


public class LSSlotName implements SlotName {
	private LSSlotNameEnum en;
	private int typeNum = -1;
	public LSSlotName(LSSlotNameEnum en) {
		this.en = en;
		switch(en) {
			case Word:
				break;
			default:
				throw new NotImplementedException();	
		}
	}
	public LSSlotName(LSSlotNameEnum en, int typeNum) {
		this.en = en;
		switch(en) {
			case Combinator:
				this.typeNum = typeNum;
				break;
			case LeftType:
				this.typeNum = typeNum;
				break;
			case RightType:
				this.typeNum = typeNum;
				break;
			case Type:
				this.typeNum = typeNum;
				break;
			default:
				throw new NotImplementedException();		
		}
	}
	public String toString() {
		String def = en.toString();
		if (typeNum != -1) {
			def += typeNum;
		}
		return def;
	}
}
