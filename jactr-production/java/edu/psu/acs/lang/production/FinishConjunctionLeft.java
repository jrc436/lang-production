package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.sentence.SSlotName;
import edu.psu.acs.lang.declarative.sentence.SSlotNameEnum;
import edu.psu.acs.lang.declarative.slot.ISlot;
import edu.psu.acs.lang.declarative.slot.Slot;
import edu.psu.acs.lang.declarative.slot.SlotVar;
import edu.psu.acs.lang.declarative.type.ConjEnum;
import edu.psu.acs.lang.declarative.type.Conjable;

public class FinishConjunctionLeft extends SyntaxRule {
	//not called "resolve" because it is a bona fide rule. It represents merging something with 'and poop', for instance, 'butts'
	//since right now the conjunction is on the left, the new thing should also be on the left
	public FinishConjunctionLeft(int leftNum, int leftTypeNum, int rightNum, int rightTypeNum, int maxtypes, int maxwords) {
		super(SyntaxRuleType.CONJDL, leftNum, leftTypeNum, rightNum, rightTypeNum, maxwords, maxtypes);
		//we also have to make sure they have the same type! at least, I'm pretty sure that's how conjunctions work....
		String type = "A";
		String conjWord = "rword";
		String fullWord = "lword";
		List<ISlot> goalPrecond = new ArrayList<ISlot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, leftNum, maxwords, leftTypeNum, maxtypes), new SlotVar(type)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, rightNum, maxwords, rightTypeNum, maxtypes), new SlotVar(type)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, rightNum, maxwords, rightTypeNum, maxtypes), new Conjable(ConjEnum.Conjable)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, leftNum, maxwords), new SlotVar(fullWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, rightNum, maxwords), new SlotVar(conjWord)));
		this.requireSentence(goalPrecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> effects = new ArrayList<ISlot>();
		effects.addAll(ProductionRule.wipeOut(leftNum, maxwords, maxtypes));
		effects.addAll(ProductionRule.cleanNonCombinedTypes(rightNum, rightTypeNum, maxwords, maxtypes));
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, rightNum, maxwords, rightTypeNum, maxtypes), new Conjable(ConjEnum.Nonconjable)));
		this.modifyGoal(effects);
		
		this.addOutput(SyntaxRule.getOutput(conjWord, fullWord, rightNum, leftNum, false));
	}

}
