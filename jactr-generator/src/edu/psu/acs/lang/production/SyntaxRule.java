package edu.psu.acs.lang.production;

import java.util.ArrayList;

public class SyntaxRule extends ProductionRule {

	public SyntaxRule(SyntaxRuleType typ, int cueNum, int cueTypeNum) {
		super(typ.toString()+cueNum+cueTypeNum, new ArrayList<BufferConditions>(), new ArrayList<BufferEffects>(), new ArrayList<String>());
	}

}
