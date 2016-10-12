package edu.psu.acs.lang.declarative.lexsyn;

public enum LSSlotNameEnum {
	Word, //string itself
	Type, //full type of the word
	LeftType, //left portion of the type
	RightType, //right portion of the type
	Conj,
	Combinator; //what holds the left and right together
}
