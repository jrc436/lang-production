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

public class ForwardComposition extends SyntaxRule {

	public ForwardComposition(int cueNum, int cueTypeNum, int maxtypes) {
		super(SyntaxRuleType.ForwardComposition, cueNum, cueTypeNum);
		
		String rightslot = "A";
		String leftslot = "C";
		String cueRight = "B";
		String word = "word";
		
		List<Slot> goalPrecond = new ArrayList<Slot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new SlotVar(rightslot)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new SlotVar(leftslot)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), CCGOperator.Slash));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.CueCombo, cueNum, cueTypeNum), CCGOperator.Slash));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.CueRightType, cueNum, cueTypeNum), new SlotVar(cueRight)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.CueLeftType, cueNum, cueTypeNum), new SlotVar(rightslot)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.Cue, cueNum), new SlotVar(word)));
		super.conditions.add(new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, goalPrecond));
		
		List<Slot> goalmod = new ArrayList<Slot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new NullValue()));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new SlotVar(cueRight)));
		goalmod.addAll(SyntaxRule.wipeOut(cueNum, maxtypes));
		super.effects.add(BufferEffects.modifyGoal(goalmod));
		
		List<Slot> makeret = new ArrayList<Slot>();
		makeret.add(new Slot(CCGTypeSlot.LeftType, new SlotVar(leftslot)));
		makeret.add(new Slot(CCGTypeSlot.RightType, new SlotVar(cueRight)));
		makeret.add(new Slot(CCGTypeSlot.Combinator, CCGOperator.Slash));
		super.effects.add(BufferEffects.makeRetrieval(ChunkTypeEnum.CCGType, makeret));
		
		super.outputs.add(new SlotVar(word).toString()+" was added to the right");
	}

}
