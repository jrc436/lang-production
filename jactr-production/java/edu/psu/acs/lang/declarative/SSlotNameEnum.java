package edu.psu.acs.lang.declarative;

//please use SSlotName, sorry for inconvenience!
public enum SSlotNameEnum {
	Complete,
	WordSem, //word is from the "wordbag"
	LexsynString, //cue is from the "cuelist" ... they are both equal-valued, but the transfer from word->cue requires an operation
	LexsynFullType,
	LexsynLeftType,
	LexsynRightType,
	LexsynCombo,
	LexsynConj;
}
