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

public class BackwardComposition extends SyntaxRule {

	public BackwardComposition(int cueNum, int cueTypeNum, int maxTypes) {
		super(SyntaxRuleType.BackwardComposition, cueNum, cueTypeNum);
		
		String rightfull = "A";
		String leftfull = "C"; //also cue right
		String cueLeft = "D";
		String word = "word";
		
		List<Slot> goalprecond = new ArrayList<Slot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new SlotVar(rightfull)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new SlotVar(leftfull)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), CCGOperator.Backslash));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueCombo, cueNum, cueTypeNum), CCGOperator.Backslash));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueLeftType, cueNum, cueTypeNum), new SlotVar(cueLeft)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueRightType, cueNum, cueTypeNum), new SlotVar(leftfull)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.Cue, cueNum), new SlotVar(word)));
		super.conditions.add(new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, goalprecond));
		
		List<Slot> goalmod = new ArrayList<Slot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new NullValue()));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new SlotVar(cueLeft)));
		goalmod.addAll(SyntaxRule.wipeOut(cueNum, maxTypes));
		super.effects.add(BufferEffects.modifyGoal(goalmod));
		
		List<Slot> makeret = new ArrayList<Slot>();
		makeret.add(new Slot(CCGTypeSlot.RightType, new SlotVar(rightfull)));
		makeret.add(new Slot(CCGTypeSlot.LeftType, new SlotVar(cueLeft)));
		makeret.add(new Slot(CCGTypeSlot.Combinator, CCGOperator.Backslash));
		super.effects.add(BufferEffects.makeRetrieval(ChunkTypeEnum.CCGType, makeret));
		
		super.outputs.add(new SlotVar(word).toString()+" was added to the left");
		
	}

}
