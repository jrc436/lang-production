package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.CCGOperator;
import edu.psu.acs.lang.declarative.CCGTypeSlot;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class ResolveForwardComposition extends SyntaxRuleResolution {
	public ResolveForwardComposition() {
		super(SyntaxRuleType.ForwardComposition);
		String right = "A";
		String left = "C";
		String rettype = "T";
		List<Slot> goalprecond = new ArrayList<Slot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new SlotVar(right)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new SlotVar(left)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), CCGOperator.Slash));
		super.conditions.add(new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, goalprecond));
		
		List<Slot> retprecond = new ArrayList<Slot>();
		retprecond.add(new Slot(CCGTypeSlot.FullType, new SlotVar(rettype)));
		retprecond.add(new Slot(CCGTypeSlot.Combinator, CCGOperator.Slash));
		retprecond.add(new Slot(CCGTypeSlot.RightType, new SlotVar(right)));
		retprecond.add(new Slot(CCGTypeSlot.LeftType, new SlotVar(left)));
		super.conditions.add(new BufferConditions(Buffer.retrieval, ChunkTypeEnum.CCGType, retprecond));
		
		List<Slot> goalmod = new ArrayList<Slot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new SlotVar(rettype)));
		super.effects.add(BufferEffects.modifyGoal(goalmod));
	}

}
