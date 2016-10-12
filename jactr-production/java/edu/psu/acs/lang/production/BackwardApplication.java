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

public class BackwardApplication extends SyntaxRule {
	public BackwardApplication(int lexsynLeftNum, int leftTypeNum, int lexsynRightNum, int rightTypeNum, int maxTypes, int maxNumWords) {
		super(SyntaxRuleType.BackwardApplication, lexsynLeftNum, leftTypeNum, lexsynRightNum, rightTypeNum, maxNumWords, maxTypes);
		String leftWord = "lword";
		String rightWord = "rword";
		String rightLeftType = "A"; //B < A\B = A 
		String leftFullType = "B";
		List<ISlot> goalPrecond = new ArrayList<ISlot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynLeftNum, maxNumWords, leftTypeNum, maxTypes), new SlotVar(leftFullType)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynRightNum, maxNumWords, rightTypeNum, maxTypes), new CCGOperator(CCGOperatorEnum.Backslash)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, lexsynLeftNum, maxNumWords), new SlotVar(leftWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, lexsynRightNum, maxNumWords), new SlotVar(rightWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynRightNum, maxNumWords, rightTypeNum, maxTypes), new SlotVar(rightLeftType)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynRightNum, maxNumWords, rightTypeNum, maxTypes), new SlotVar(leftFullType)));
		
		requireSentence(goalPrecond);
		makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> effects = new ArrayList<ISlot>();
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynRightNum, maxNumWords, rightTypeNum, maxTypes), new NullValue()));
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynRightNum, maxNumWords, rightTypeNum, maxTypes), new NullValue()));
		effects.addAll(ProductionRule.wipeOut(lexsynLeftNum, maxNumWords, maxTypes));
		effects.addAll(ProductionRule.cleanNonCombinedTypes(lexsynRightNum, rightTypeNum, maxNumWords, maxTypes));
		
		modifyGoal(effects);
		
		List<ISlot> retr = new ArrayList<ISlot>();
		retr.add(new Slot(CCGTypeSlot.FullType, new SlotVar(rightLeftType)));
		
		makeRetrieval(ChunkTypeEnum.CCGType, retr);	
		
		this.addOutput(SyntaxRule.getOutput(rightWord, leftWord, lexsynRightNum, lexsynLeftNum, false));
	}

}
