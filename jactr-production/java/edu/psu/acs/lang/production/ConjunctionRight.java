package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.sentence.SSlotName;
import edu.psu.acs.lang.declarative.sentence.SSlotNameEnum;
import edu.psu.acs.lang.declarative.slot.ISlot;
import edu.psu.acs.lang.declarative.slot.Slot;
import edu.psu.acs.lang.declarative.slot.SlotVar;
import edu.psu.acs.lang.declarative.type.CCGBaseType;
import edu.psu.acs.lang.declarative.type.CCGBaseTypeEnum;
import edu.psu.acs.lang.declarative.type.ConjEnum;
import edu.psu.acs.lang.declarative.type.Conjable;

//this rule refers to combining a conjunction with a non-conjunction.
public class ConjunctionRight extends SyntaxRule {
	
	//consider that the combinatory rule that combines 'poop' to the left of 'and' is different than the combinatory rule that combines 'poop' to the
	//right of 'and', what's not entirely clear is if both of these combinatory rules exist
	//ConjunctionRight represents adding 'and' to the right side of poop
	public ConjunctionRight(int leftNum, int leftTypeNum, int rightNum, int rightTypeNum, int maxtypes, int maxwords) {
		super(SyntaxRuleType.CONJR, leftNum, leftTypeNum, rightNum, rightTypeNum, maxwords, maxtypes);
		String conjWord = "rword";
		String fullWord = "lword";
		
		List<ISlot> goalPrecond = new ArrayList<ISlot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, rightNum, maxwords, rightTypeNum, maxtypes), CCGBaseType.makeType(CCGBaseTypeEnum.conj, null)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, leftNum, maxwords), new SlotVar(fullWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, rightNum, maxwords), new SlotVar(conjWord)));
		this.requireSentence(goalPrecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> effects = new ArrayList<ISlot>();
		effects.addAll(ProductionRule.wipeOut(rightNum, maxwords, maxtypes));
		effects.addAll(ProductionRule.cleanNonCombinedTypes(leftNum, leftTypeNum, maxwords, maxtypes));
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, leftNum, maxwords, leftTypeNum, maxtypes), new Conjable(ConjEnum.Conjable)));
		this.modifyGoal(effects);
		
		this.addOutput(SyntaxRule.getOutput(conjWord, fullWord, rightNum, leftNum, true));
	}

}
