package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class ChooseFirstWord extends ProductionRule {
	public ChooseFirstWord(int wordIndex, int maxTypes) {
		super("chooseFirstWord"+wordIndex, new ArrayList<BufferConditions>(), new ArrayList<BufferEffects>(), new ArrayList<String>());
		
		String cueVar = "first";
		String typeVar = "type";
		String leftVar = "ltype";
		String rightVar = "rtype";
		String cueCombo = "combo";
		
		List<Slot> goalprecond = new ArrayList<Slot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.Cue, wordIndex), new SlotVar(cueVar)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueType, wordIndex, 1), new SlotVar(typeVar)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueLeftType, wordIndex, 1), new SlotVar(leftVar)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueRightType, wordIndex, 1), new SlotVar(rightVar)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueCombo, wordIndex, 1), new SlotVar(cueCombo)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueType, wordIndex, 2), new NullValue())); //we really want a simple first word...
		super.conditions.add(new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, goalprecond));
		
		List<Slot> goalmod = new ArrayList<Slot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new SlotVar(typeVar)));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new SlotVar(leftVar)));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new SlotVar(rightVar)));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), new SlotVar(cueCombo)));
		goalmod.addAll(ProductionRule.wipeOut(wordIndex, maxTypes));
		super.effects.add(BufferEffects.modifyGoal(goalmod));
		
		super.outputs.add("Choosing "+new SlotVar(cueVar).toString()+" for our first word!");
	}

}
