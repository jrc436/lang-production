package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.chunk.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.lexsyn.LSSlotName;
import edu.psu.acs.lang.declarative.lexsyn.LSSlotNameEnum;
import edu.psu.acs.lang.declarative.sentence.SSlotName;
import edu.psu.acs.lang.declarative.sentence.SSlotNameEnum;
import edu.psu.acs.lang.declarative.slot.ISlot;
import edu.psu.acs.lang.declarative.slot.NullValue;
import edu.psu.acs.lang.declarative.slot.Slot;
import edu.psu.acs.lang.declarative.slot.SlotVar;

public class GrabWord extends ProductionRule {

	//we now need maxwords * WM grabwords.
	public GrabWord(int wordNum, int maxwords, int memNum, int maxMem) {
		super("GrabWord"+wordNum+"-"+memNum);
		List<ISlot> goalprecond = new ArrayList<ISlot>();
		String wordVar = "word";
		
		//goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.Free)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.WordSem, wordNum, maxwords), new SlotVar(wordVar)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, memNum, maxMem), new NullValue()));
		this.requireSentence(goalprecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> goalmod = new ArrayList<ISlot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.WordSem, wordNum, maxwords), new NullValue()));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, memNum, maxMem), new SlotVar(wordVar)));
		//goalmod.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.RetrievedLexSyn)));
		
		List<ISlot> ret = new ArrayList<ISlot>();
		ret.add(new Slot(new LSSlotName(LSSlotNameEnum.Word), new SlotVar(wordVar)));
		
		this.modifyGoal(goalmod);
		this.makeRetrieval(ChunkTypeEnum.lexsyn, ret);
	}

}
