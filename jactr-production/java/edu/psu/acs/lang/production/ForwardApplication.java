package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.CCGOperator;
import edu.psu.acs.lang.declarative.CCGOperatorEnum;
import edu.psu.acs.lang.declarative.CCGTypeSlot;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class ForwardApplication extends SyntaxRule {
	public ForwardApplication(int lexsynLeftNum, int leftTypeNum, int lexsynRightNum, int rightTypeNum, int maxTypes, int maxwords) {
		super(SyntaxRuleType.ForwardApplication, lexsynLeftNum, leftTypeNum, lexsynRightNum, rightTypeNum, maxwords, maxTypes);
		String rightWord = "rword";
		String leftWord = "lword";
		String rightFullType = "B"; //A/B > B = A
		String leftLeftType = "A";
		List<ISlot> goalPrecond = new ArrayList<ISlot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new SlotVar(rightFullType)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new SlotVar(leftLeftType)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynLeftNum, maxwords, rightTypeNum, maxTypes), new CCGOperator(CCGOperatorEnum.Slash)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, lexsynLeftNum, maxwords), new SlotVar(leftWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, lexsynRightNum, maxwords), new SlotVar(rightWord)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynRightNum, maxwords, rightTypeNum, maxTypes), new SlotVar(rightFullType)));

		this.requireSentence(goalPrecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> effects = new ArrayList<ISlot>();
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new NullValue()));
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynLeftNum, maxwords, leftTypeNum, maxTypes), new NullValue()));

		effects.addAll(ProductionRule.wipeOut(lexsynRightNum, maxwords, maxTypes));
		effects.addAll(ProductionRule.cleanNonCombinedTypes(lexsynLeftNum, leftTypeNum, maxwords, maxTypes));
		
		this.modifyGoal(effects);
		
		List<ISlot> retr = new ArrayList<ISlot>();
		retr.add(new Slot(CCGTypeSlot.FullType, new SlotVar(leftLeftType)));
		
		this.makeRetrieval(ChunkTypeEnum.CCGType, retr);
		
		this.addOutput(SyntaxRule.getOutput(rightWord, leftWord, lexsynRightNum, lexsynLeftNum, true));
	}

}
