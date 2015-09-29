package edu.psu.acs.lang.declarative;

//please use SSlotName, sorry for inconvenience!
public enum SSlotNameEnum {
	FullType,
	LeftFullType,
	RightFullType,
	Combinator,
	Word, //word is from the "wordbag"
	Cue, //cue is from the "cuelist" ... they are both equal-valued, but the transfer from word->cue requires an operation
	CueType,
	CueLeftType,
	CueRightType,
	CueCombo;
}
