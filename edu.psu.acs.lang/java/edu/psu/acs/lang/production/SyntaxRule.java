package edu.psu.acs.lang.production;

import edu.psu.acs.lang.declarative.CCGBaseType;
import edu.psu.acs.lang.declarative.CCGCompoundType;
import edu.psu.acs.lang.declarative.CCGOperator;
import edu.psu.acs.lang.declarative.CCGOperatorEnum;
import edu.psu.acs.lang.declarative.CCGType;

public abstract class SyntaxRule extends ProductionRule {

	public SyntaxRule(SyntaxRuleType typ, int leftNum, int leftTypeNum, int rightNum, int rightTypeNum, int maxwords, int maxtypes) {
		super(typ.getName()+String.format("%0"+String.valueOf(maxwords).length()+"d", leftNum)+"-"+String.format("%0"+String.valueOf(maxtypes).length()+"d", leftTypeNum)+"--"+String.format("%0"+String.valueOf(maxwords).length()+"d", rightNum)+"-"+String.format("%0"+String.valueOf(maxtypes).length()+"d", rightTypeNum));
	}
	public static CCGType BApp(CCGType left, CCGType right) {
		if (!(left instanceof CCGCompoundType)) {
			return null;
		}
		if (left.getCombo().equals(new CCGOperator(CCGOperatorEnum.Backslash)) && left.getRight().equals(right)) {
			if (left.getLeft().getCombo() == null) {
				return new CCGBaseType((CCGBaseType)left.getLeft());
			}
			return CCGCompoundType.makeType(left.getLeft().getLeft(), left.getLeft().getRight(), left.getLeft().getCombo());
		}
		return null;
	}
	public static CCGType FApp(CCGType left, CCGType right) {
		if (!(left instanceof CCGCompoundType)) {
			return null;
		}
		if (left.getCombo().equals(new CCGOperator(CCGOperatorEnum.Slash)) && left.getRight().equals(right)) {
			if (left.getLeft().getCombo() == null) {
				return new CCGBaseType((CCGBaseType)left.getLeft());
			}
			return CCGCompoundType.makeType(left.getLeft().getLeft(), left.getLeft().getRight(), left.getLeft().getCombo());
		}
		return null;
	}
	public static CCGType FComp(CCGType left, CCGType right) {
		if (!(left instanceof CCGCompoundType)|| !(right instanceof CCGCompoundType)) {
			return null;
		}
		if (left.getCombo().equals(new CCGOperator(CCGOperatorEnum.Slash)) && left.getCombo().equals(new CCGOperator(CCGOperatorEnum.Slash)) && left.getRight().equals(right.getLeft())) {
			return CCGCompoundType.makeType(left.getLeft(), right.getRight(), new CCGOperator(CCGOperatorEnum.Slash));
		}
		return null;
	}
	public static CCGType BComp(CCGType left, CCGType right) {
		if (!(left instanceof CCGCompoundType) || !(right instanceof CCGCompoundType)) {
			return null;
		}
		if (left.getCombo().equals(new CCGOperator(CCGOperatorEnum.Backslash)) && right.getCombo().equals(new CCGOperator(CCGOperatorEnum.Backslash)) && left.getLeft().equals(right.getRight())) {
			return CCGCompoundType.makeType(right.getLeft(), left.getRight(), new CCGOperator(CCGOperatorEnum.Backslash));
		}
		return null;
	}
}
