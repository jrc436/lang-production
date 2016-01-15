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

public class ResolveBackwardComposition extends SyntaxRuleResolution {

	public ResolveBackwardComposition(int lexsynNum, int lexsynTypeNum, int maxwords, int maxTypes) {
		super(SyntaxRuleType.BackwardComposition, lexsynNum, lexsynTypeNum, maxwords, maxTypes);
		
		String right = "A";
		String left = "D";
		String rettype = "T";
		
		List<ISlot> goalprecond = new ArrayList<ISlot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new SlotVar(right)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new SlotVar(left)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new CCGOperator(CCGOperatorEnum.Backslash)));
	//	goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.RetrievedRule)));
		this.requireGoal(goalprecond);
		
		List<ISlot> retprecond = new ArrayList<ISlot>();
		retprecond.add(new Slot(CCGTypeSlot.FullType, new SlotVar(rettype)));
		retprecond.add(new Slot(CCGTypeSlot.Combinator, new CCGOperator(CCGOperatorEnum.Backslash)));
		retprecond.add(new Slot(CCGTypeSlot.RightType, new SlotVar(right)));
		retprecond.add(new Slot(CCGTypeSlot.LeftType, new SlotVar(left)));
		this.requireRetrieval(retprecond, ChunkTypeEnum.CCGType);
		
		List<ISlot> goalmod = new ArrayList<ISlot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new SlotVar(rettype)));
		//goalmod.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.Free)));
		this.modifyGoal(goalmod);
	}

}
