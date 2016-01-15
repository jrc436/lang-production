package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import edu.psu.acs.lang.IModelElement;

public class ParameterSet implements IModelElement {
	private Set<IParameter> params;
	public ParameterSet(Set<IParameter> params) {
		this.params = params;
	}
	
	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		lines.add("<parameters>");
		for (IParameter p : params) {
			lines.add(p.toXML());
		}
		lines.add("</parameters>");
		return lines;
	}

}
