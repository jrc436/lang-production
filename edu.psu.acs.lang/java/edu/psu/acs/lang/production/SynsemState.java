package edu.psu.acs.lang.production;

public enum SynsemState {
	start,
	realizeClause,
	realizeRetrieving,
	realizeRetrievingSyn,
	retrievingArgOrder,
	realize,
	realizedFuncWord,
	adjoin,
	realizeNextArg,
	argPicked,
	argRealized,
	realizeBackTrack,
	splitType,
	adjoin2,
	free,
	adjoined;
}
