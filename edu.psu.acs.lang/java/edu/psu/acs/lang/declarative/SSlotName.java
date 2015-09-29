package edu.psu.acs.lang.declarative;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;


//sentence slot name, for modularity
public class SSlotName implements SlotName {
	private SSlotNameEnum en;
	private int cueTypeNum = -1;
	private int cueNum = -1;
	public SSlotName(SSlotNameEnum en, int cueNum, int cueTypeNum) {
		this.en = en;
		switch(en) {
			case CueCombo:				
				this.cueNum = cueNum;
				this.cueTypeNum = cueTypeNum;
				break;
			case CueLeftType:
				this.cueNum = cueNum;
				this.cueTypeNum = cueTypeNum;
				break;
			case CueRightType:
				this.cueNum = cueNum;
				this.cueTypeNum = cueTypeNum;
				break;
			case CueType:
				this.cueNum = cueNum;
				this.cueTypeNum = cueTypeNum;
				break;		
			default:
				throw new NotImplementedException();		
		}
	}
	public SSlotName(SSlotNameEnum en, int cueNum) {
		this.en = en;
		switch(en) {		
			case Cue:				
				this.cueNum = cueNum;
				break;	
			case Word:
				this.cueNum = cueNum;
				break;	
		default:
			throw new NotImplementedException();				
		}
	}
	public SSlotName(SSlotNameEnum en) {
		this.en = en;
		switch (en) {
			case Combinator:			
				break;		
			case FullType:
				break;
			case LeftFullType:
				break;
			case RightFullType:
				break;
			default:
				throw new NotImplementedException();
		}
	}
	public String toString() {
		String def = en.toString();
		if (cueNum != -1) {
			def += cueNum;
		}
		if (cueTypeNum != -1) {
			def += cueTypeNum;
		}
		return def;
	}
}
