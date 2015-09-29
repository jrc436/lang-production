package edu.psu.acs.david;

import edu.psu.acs.lang.declarative.SlotValue;

/**
 * I am not sure if all of the things here should actually be on the same list, but they all are related to syntypes
 * @author jrc
 *
 */
public enum CCGSyntypeEnum implements SlotValue {
	NP,
	NPTypeRaised,
	PPTo,
	VP,
	S,
	InTrans,
	Trans,
	Transto,
	DiTrans,
	DiTransTo,
	PrepTo,
	sfnp,
	sfppto,
	sfnpfnp,
	sfnpfppto,
	sfpptofnp,
	adjunct1,
	LexPrepto,
	Nullsyn;
}
