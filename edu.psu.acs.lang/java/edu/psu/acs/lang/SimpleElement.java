package edu.psu.acs.lang;

import java.util.ArrayList;
import java.util.List;

public class SimpleElement implements IModelElement {
	private List<String> token = new ArrayList<String>();
	public SimpleElement(String simpleToken) {
		this.token.add(simpleToken);
	}
	@Override
	public List<String> toXML() {
		return token;
	}

}
