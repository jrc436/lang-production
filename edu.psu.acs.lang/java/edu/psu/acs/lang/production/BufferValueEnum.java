package edu.psu.acs.lang.production;

public enum BufferValueEnum implements BufferQueryEnum {
	full,
	empty,
	requested,
	unrequested;

	@Override
	public String type() {
		return "buffer";
	}
}
