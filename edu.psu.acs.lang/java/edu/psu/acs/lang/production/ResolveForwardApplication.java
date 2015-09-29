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

public class ResolveForwardApplication extends SyntaxRuleResolution {

	public ResolveForwardApplication() {
		super(SyntaxRuleType.ForwardApplication);
		
		String type = "A";
		String retLeft = "leftA";
		String retRight = "rightA";
		String retCombo = "comboA";
		
		List<Slot> goalprecond = new ArrayList<Slot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new SlotVar(type)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), CCGOperator.Slash));
		super.conditions.add(new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, goalprecond));
		
		List<Slot> retprecond = new ArrayList<Slot>();
		retprecond.add(new Slot(CCGTypeSlot.FullType, new SlotVar(type)));
		retprecond.add(new Slot(CCGTypeSlot.LeftType, new SlotVar(retLeft)));
		retprecond.add(new Slot(CCGTypeSlot.RightType, new SlotVar(retRight)));
		retprecond.add(new Slot(CCGTypeSlot.Combinator, new SlotVar(retCombo)));
		super.conditions.add(new BufferConditions(Buffer.retrieval, ChunkTypeEnum.CCGType, retprecond));
		
		List<Slot> goalActions = new ArrayList<Slot>();
		goalActions.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new SlotVar(retLeft)));
		goalActions.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new SlotVar(type)));
		goalActions.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new SlotVar(retRight)));
		goalActions.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), new SlotVar(retCombo)));
		super.effects.add(BufferEffects.modifyGoal(goalActions));
	}

}
