package edu.psu.acs.lang.production;


public class SyntaxRuleResolution extends ProductionRule {

	public SyntaxRuleResolution(SyntaxRuleType typ, int lexsynNum, int lexsynType, int maxwords, int maxtypes) {
		super(typ.getName()+"Resolve"+String.format("%0"+String.valueOf(maxwords).length()+"d", lexsynNum)+"-"+String.format("%0"+String.valueOf(maxtypes).length()+"d", lexsynType));
	}

}
