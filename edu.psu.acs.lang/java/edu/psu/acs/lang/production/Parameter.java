package edu.psu.acs.lang.production;

public class Parameter implements IParameter {
	private ParameterNameEnum name;
	private String value;
	public Parameter(ParameterNameEnum name, String Value) {
		this.name = name;
		this.value = Value;
	}

	public String toXML() {
		return "<parameter name=\""+name.toString()+"\" value=\""+value+"\"/>";
	}
	
}
