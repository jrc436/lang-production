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

public class BackwardApplication extends SyntaxRule {
	public BackwardApplication(int cueNum, int cueTypeNum, int maxTypes) {
		super(SyntaxRuleType.BackwardApplication, cueNum, cueTypeNum);
		String word = "word";
		String cuetype = "A";
		List<Slot> goalPrecond = new ArrayList<Slot>();
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new SlotVar(cuetype)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), CCGOperator.Backslash));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.Cue, cueNum), new SlotVar(word)));
		goalPrecond.add(new Slot(new SSlotName(SSlotNameEnum.CueType, cueNum, cueTypeNum), new SlotVar(cuetype)));
		BufferConditions goal = new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, goalPrecond);
		super.conditions.add(goal);
		
		List<Slot> effects = new ArrayList<Slot>();
		effects.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new NullValue()));
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new NullValue()));
		effects.addAll(ProductionRule.wipeOut(cueNum, maxTypes));
		super.effects.add(BufferEffects.modifyGoal(effects));
		
		List<Slot> retr = new ArrayList<Slot>();
		retr.add(new Slot(CCGTypeSlot.FullType, new SlotVar(cuetype)));
		super.effects.add(BufferEffects.makeRetrieval(ChunkTypeEnum.CCGType, retr));
		
		super.outputs.add(new SlotVar(word).toString()+" was added to the left");
	}
}
