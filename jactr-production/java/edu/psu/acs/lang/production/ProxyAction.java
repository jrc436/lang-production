package edu.psu.acs.lang.production;

import java.util.List;

import edu.psu.acs.lang.declarative.slot.ISlot;

public class ProxyAction extends BufferEffects {
	//note that classname has no error checking done. 
	public ProxyAction(String classname, List<ISlot> vars) {
		super(null, vars, null);
	}

}
