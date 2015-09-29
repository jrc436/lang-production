package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.LSSlotName;
import edu.psu.acs.lang.declarative.LSSlotNameEnum;
import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class GrabWord extends ProductionRule {

	public GrabWord(int wordNum) {
		super("GrabWord"+wordNum, new ArrayList<BufferConditions>(), new ArrayList<BufferEffects>(), new ArrayList<String>());
		List<Slot> goalprecond = new ArrayList<Slot>();
		String wordVar = "word";
		
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.Word, wordNum), new SlotVar(wordVar)));
		super.conditions.add(new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, goalprecond));
		
		List<Slot> goalmod = new ArrayList<Slot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.Word, wordNum), new NullValue()));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.Cue, wordNum), new SlotVar(wordVar)));
		
		List<Slot> ret = new ArrayList<Slot>();
		ret.add(new Slot(new LSSlotName(LSSlotNameEnum.Word), new SlotVar(wordVar)));
		
		super.effects.add(BufferEffects.modifyGoal(goalmod));
		super.effects.add(BufferEffects.makeRetrieval(ChunkTypeEnum.lexsyn, ret));
	}

}
