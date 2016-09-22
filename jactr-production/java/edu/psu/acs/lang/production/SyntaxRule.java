package edu.psu.acs.lang.production;

import edu.psu.acs.lang.declarative.CCGBaseType;
import edu.psu.acs.lang.declarative.CCGCompoundType;
import edu.psu.acs.lang.declarative.CCGOperator;
import edu.psu.acs.lang.declarative.CCGOperatorEnum;
import edu.psu.acs.lang.declarative.CCGType;
import edu.psu.acs.lang.declarative.SlotVar;

public abstract class SyntaxRule extends ProductionRule {
	public static final String leftOf = ">>>";
	public static final String rightOf = "<<<";
	public static final String wordSep = " :: ";
	protected static String getOutput(String rightWord, String leftWord, int lexsynRightNum, int lexsynLeftNum, boolean rightJustAdded) {
		String rightString = new SlotVar(rightWord).toString() + wordSep + lexsynRightNum;
		String leftString = new SlotVar(leftWord).toString() + wordSep + lexsynLeftNum;
		return rightJustAdded ? rightString + " " + rightOf + " " + leftString : leftString + " " + leftOf + " " + rightString;
	}
	
	public SyntaxRule(SyntaxRuleType typ, int leftNum, int leftTypeNum, int rightNum, int rightTypeNum, int wmsize, int maxtypes) {
		super(typ.getName()+String.format("%0"+String.valueOf(wmsize).length()+"d", leftNum)+"-"+String.format("%0"+String.valueOf(maxtypes).length()+"d", leftTypeNum)+"--"+String.format("%0"+String.valueOf(wmsize).length()+"d", rightNum)+"-"+String.format("%0"+String.valueOf(maxtypes).length()+"d", rightTypeNum));
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
