package org.jactr.core.module.procedural.sixtrace;

import org.jactr.core.production.CannotInstantiateException;
import org.jactr.core.production.IInstantiation;
import org.jactr.core.production.VariableBindings;
import org.jactr.core.production.action.DefaultAction;
import org.jactr.core.production.action.IAction;

public class CombineString extends DefaultAction {

	@Override
	public double fire(IInstantiation instantiation, double firingTime) {
		VariableBindings vb = instantiation.getVariableBindings();
		
		String lword = ""+vb.get("=lword");
		String rword = ""+vb.get("=rword");
//		instantiation.getProdu
		return 0;
	}

	@Override
	public IAction bind(VariableBindings variableBindings) throws CannotInstantiateException {
		return new CombineString();
	}

}
