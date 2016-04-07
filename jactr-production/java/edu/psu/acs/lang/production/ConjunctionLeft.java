package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.CCGBaseType;
import edu.psu.acs.lang.declarative.CCGBaseTypeEnum;
import edu.psu.acs.lang.declarative.ConjEnum;
import edu.psu.acs.lang.declarative.Conjable;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

//consider that the combinatory rule that combines 'poop' to the left of 'and' is different than the combinatory rule that combines 'poop' to the
//right of 'and', what's not entirely clear is if both of these combinatory rules exist
//COnjunctionLeft represents adding 'and' to the left side of 'poop'
public class ConjunctionLeft extends SyntaxRule {
	public ConjunctionLeft(int leftNum, int leftTypeNum, int rightNum, int rightTypeNum, int maxtypes, int maxwords) {
		super(SyntaxRuleType.CONJL, leftNum, leftTypeNum, rightNum, rightTypeNum, maxwords, maxtypes);
		String conjWord = "lword";
		String fullWord = "rword";
		
		List<ISlot> goalPrecond = new ArrayList<ISlot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, leftNum, maxwords, leftTypeNum, maxtypes), CCGBaseType.makeType(CCGBaseTypeEnum.conj, null)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, leftNum, maxwords), new SlotVar(conjWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, rightNum, maxwords), new SlotVar(fullWord)));
		this.requireSentence(goalPrecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> effects = new ArrayList<ISlot>();
		effects.addAll(ProductionRule.wipeOut(leftNum, maxwords, maxtypes));
		effects.addAll(ProductionRule.cleanNonCombinedTypes(rightNum, rightTypeNum, maxwords, maxtypes));
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, rightNum, maxwords, rightTypeNum, maxtypes), new Conjable(ConjEnum.Conjable)));
		this.modifyGoal(effects);
		
		this.addOutput(SyntaxRule.getOutput(fullWord, conjWord, rightNum, leftNum, false));
	}
	
	
}
