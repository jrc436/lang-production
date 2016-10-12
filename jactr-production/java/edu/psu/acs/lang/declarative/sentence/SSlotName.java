package edu.psu.acs.lang.declarative.sentence;

import edu.psu.acs.lang.declarative.slot.SlotName;

//sentence slot name, for modularity
public class SSlotName implements SlotName {
	private SSlotNameEnum en;
	private String cueTypeNum = "";
	private String cueNum = "";
	public SSlotName(SSlotNameEnum en, int cueNum, int maxWords, int cueTypeNum, int maxTypes) {
		this.en = en;
		int wordDigits = String.valueOf(maxWords).length();
		String formatterI = "%0"+wordDigits+"d";
		int typesDigits = String.valueOf(maxTypes).length();
		String formatterJ = "%0"+typesDigits+"d";
		String formattedI = String.format(formatterI, cueNum);
		String formattedJ = String.format(formatterJ, cueTypeNum);
		switch(en) {
			case LexsynCombo:				
				this.cueNum = formattedI;
				this.cueTypeNum = formattedJ;
				break;
			case LexsynLeftType:
				this.cueNum = formattedI;
				this.cueTypeNum = formattedJ;
				break;
			case LexsynRightType:
				this.cueNum = formattedI;
				this.cueTypeNum = formattedJ;
				break;
			case LexsynFullType:
				this.cueNum = formattedI;
				this.cueTypeNum = formattedJ;
				break;
			case LexsynConj:
				this.cueNum = formattedI;
				this.cueTypeNum = formattedJ;
				break;
			default:
				throw new UnsupportedOperationException();		
		}
	}
	public SSlotName(SSlotNameEnum en, int cueNum, int maxWords) {
		this.en = en;
		int wordDigits = String.valueOf(maxWords).length();
		String formatterI = "%0"+wordDigits+"d";
		String formattedI = String.format(formatterI, cueNum);
		switch(en) {		
			case LexsynString:				
				this.cueNum = formattedI;
				break;	
			case WordSem:
				this.cueNum = formattedI;
				break;	
		default:
			throw new UnsupportedOperationException();				
		}
	}
	public SSlotName(SSlotNameEnum en) {
		this.en = en;
		switch (en) {
			case Complete:
				break;
			default:
				throw new UnsupportedOperationException();
		}
	}
	public String toString() {
		String def = en.toString();
		if (!cueNum.isEmpty()) {
			def += cueNum;
		}
		if (!cueTypeNum.isEmpty()) {
			//cuenum is also not -1 in this case
			def += "-"+cueTypeNum;
		}
		return def;
	}
}
