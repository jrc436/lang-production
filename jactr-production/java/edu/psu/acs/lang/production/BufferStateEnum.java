package edu.psu.acs.lang.production;

public enum BufferStateEnum implements BufferQueryEnum {
	busy,
	free,
	error;
	public String type() {
		return "state";
	}
}
