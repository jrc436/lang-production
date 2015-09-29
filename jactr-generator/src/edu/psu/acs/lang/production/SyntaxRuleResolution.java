package edu.psu.acs.lang.production;

import java.util.ArrayList;

public class SyntaxRuleResolution extends ProductionRule {

	public SyntaxRuleResolution(SyntaxRuleType typ) {
		super(typ.toString()+"Resolve", new ArrayList<BufferConditions>(), new ArrayList<BufferEffects>(), new ArrayList<String>());
	}

}
