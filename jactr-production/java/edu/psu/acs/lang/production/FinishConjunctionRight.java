package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ConjEnum;
import edu.psu.acs.lang.declarative.Conjable;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class FinishConjunctionRight extends SyntaxRule {
	//not called "resolve" because it is a bona fide rule. It represents merging something with 'poop and', for instance, 'butts'
	//since right now the conjunction is on the right, the new thing should be on the right
	public FinishConjunctionRight(int leftNum, int leftTypeNum, int rightNum, int rightTypeNum, int maxtypes, int maxwords) {
		super(SyntaxRuleType.CONJDR, leftNum, leftTypeNum, rightNum, rightTypeNum, maxwords, maxtypes);
		//we also have to make sure they have the same type! at least, I'm pretty sure that's how conjunctions work....
		String type = "A";
		String conjWord = "lword";
		String fullWord = "rword";
		List<ISlot> goalPrecond = new ArrayList<ISlot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, leftNum, maxwords, leftTypeNum, maxtypes), new SlotVar(type)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, rightNum, maxwords, rightTypeNum, maxtypes), new SlotVar(type)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, rightNum, maxwords, rightTypeNum, maxtypes), new Conjable(ConjEnum.Conjable)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, leftNum, maxwords), new SlotVar(conjWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, rightNum, maxwords), new SlotVar(fullWord)));
		this.requireSentence(goalPrecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> effects = new ArrayList<ISlot>();
		effects.addAll(ProductionRule.wipeOut(leftNum, maxwords, maxtypes));
		effects.addAll(ProductionRule.cleanNonCombinedTypes(rightNum, rightTypeNum, maxwords, maxtypes));
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, rightNum, maxwords, rightTypeNum, maxtypes), new Conjable(ConjEnum.Nonconjable)));
		this.modifyGoal(effects);
		
		this.addOutput(SyntaxRule.getOutput(fullWord, conjWord, rightNum, leftNum, true));
	}
}
