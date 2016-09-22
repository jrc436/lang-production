package edu.psu.acs.lang.production;

import java.util.List;

import edu.psu.acs.lang.core.IModelElement;

public interface IProductionRule extends IModelElement {
	public List<String> toXML();
}
