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

public class BackwardComposition extends SyntaxRule {

	public BackwardComposition(int lexsynLeftNum, int leftTypeNum, int lexsynRightNum, int rightTypeNum, int maxTypes, int maxwords) {
		super(SyntaxRuleType.BackwardComposition, lexsynLeftNum, leftTypeNum, lexsynRightNum, rightTypeNum, maxwords, maxTypes);
		//A\B B< C\A = C\B
		String leftRightType = "B";
		String leftLeftRightRight = "A"; //also cue right
		String rightLeftType = "C";
		String rightWord = "rword";
		String leftWord = "lword";
		
		List<ISlot> goalprecond = new ArrayList<ISlot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new SlotVar(leftRightType)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new SlotVar(leftLeftRightRight)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new CCGOperator(CCGOperatorEnum.Backslash)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, lexsynLeftNum, maxwords), new SlotVar(leftWord)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynRightNum, maxwords, rightTypeNum, maxTypes), new SlotVar(rightLeftType)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynRightNum, maxwords, rightTypeNum, maxTypes), new SlotVar(leftLeftRightRight)));		
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynRightNum, maxwords, rightTypeNum, maxTypes), new CCGOperator(CCGOperatorEnum.Backslash)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, lexsynRightNum, maxwords), new SlotVar(rightWord)));
		
		this.requireSentence(goalprecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> goalmod = new ArrayList<ISlot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new NullValue()));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new SlotVar(rightLeftType)));
		//the left's right type is already correct, so gr8!
		goalmod.addAll(SyntaxRule.wipeOut(lexsynRightNum, maxwords, maxTypes));
		goalmod.addAll(SyntaxRule.cleanNonCombinedTypes(lexsynLeftNum, leftTypeNum, maxwords, maxTypes));
		
		this.modifyGoal(goalmod);
		
		List<ISlot> makeret = new ArrayList<ISlot>();
		makeret.add(new Slot(CCGTypeSlot.RightType, new SlotVar(leftRightType)));
		makeret.add(new Slot(CCGTypeSlot.LeftType, new SlotVar(rightLeftType)));
		makeret.add(new Slot(CCGTypeSlot.Combinator, new CCGOperator(CCGOperatorEnum.Backslash)));
		
		this.makeRetrieval(ChunkTypeEnum.CCGType, makeret);
		this.addOutput(SyntaxRule.getOutput(rightWord, leftWord, lexsynRightNum, lexsynLeftNum, false));	
	}

}
