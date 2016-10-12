package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.chunk.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.sentence.SSlotName;
import edu.psu.acs.lang.declarative.sentence.SSlotNameEnum;
import edu.psu.acs.lang.declarative.slot.ISlot;
import edu.psu.acs.lang.declarative.slot.NullValue;
import edu.psu.acs.lang.declarative.slot.Slot;
import edu.psu.acs.lang.declarative.slot.SlotVar;
import edu.psu.acs.lang.declarative.type.CCGOperator;
import edu.psu.acs.lang.declarative.type.CCGOperatorEnum;
import edu.psu.acs.lang.declarative.type.CCGTypeSlot;

public class ForwardComposition extends SyntaxRule {

	public ForwardComposition(int lexsynLeftNum, int leftTypeNum, int lexsynRightNum, int rightTypeNum, int maxtypes, int maxwords) {
		super(SyntaxRuleType.ForwardComposition, lexsynLeftNum, leftTypeNum, lexsynRightNum, rightTypeNum, maxwords, maxtypes);
		//A/B B> B/C = A/C
	
		String leftRightRightLeft = "B";
		String leftLeft = "A";
		String rightRight = "C";
		String rightWord = "rword";
		String leftWord = "lword";
		
		List<ISlot> goalPrecond = new ArrayList<ISlot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynLeftNum, maxwords, leftTypeNum, maxtypes), new SlotVar(leftRightRightLeft)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynLeftNum, maxwords, leftTypeNum, maxtypes), new SlotVar(leftLeft)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynLeftNum, maxwords, leftTypeNum, maxtypes), new CCGOperator(CCGOperatorEnum.Slash)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, lexsynLeftNum, maxwords), new SlotVar(leftWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynRightNum, maxwords, rightTypeNum, maxtypes), new SlotVar(rightRight)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynRightNum, maxwords, rightTypeNum, maxtypes), new SlotVar(leftRightRightLeft)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynRightNum, maxwords, rightTypeNum, maxtypes), new CCGOperator(CCGOperatorEnum.Slash)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, lexsynRightNum, maxwords), new SlotVar(rightWord)));
		this.requireSentence(goalPrecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> goalmod = new ArrayList<ISlot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynLeftNum, maxwords, leftTypeNum, maxtypes), new NullValue()));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynLeftNum, maxwords, leftTypeNum, maxtypes), new SlotVar(rightRight)));
		//leftLeft is already correct, so yay!
		goalmod.addAll(SyntaxRule.wipeOut(lexsynRightNum, maxwords, maxtypes));
		goalmod.addAll(SyntaxRule.cleanNonCombinedTypes(lexsynLeftNum, leftTypeNum, maxwords, maxtypes));
		this.modifyGoal(goalmod);
		
		List<ISlot> makeret = new ArrayList<ISlot>();
		makeret.add(new Slot(CCGTypeSlot.LeftType, new SlotVar(leftLeft)));
		makeret.add(new Slot(CCGTypeSlot.RightType, new SlotVar(rightRight)));
		makeret.add(new Slot(CCGTypeSlot.Combinator, new CCGOperator(CCGOperatorEnum.Slash)));
		this.makeRetrieval(ChunkTypeEnum.CCGType, makeret);
		
		this.addOutput(SyntaxRule.getOutput(rightWord, leftWord, lexsynRightNum, lexsynLeftNum, true));
	}

}
