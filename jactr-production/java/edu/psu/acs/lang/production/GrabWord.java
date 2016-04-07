package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.LSSlotName;
import edu.psu.acs.lang.declarative.LSSlotNameEnum;
import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class GrabWord extends ProductionRule {

	public GrabWord(int wordNum, int maxwords) {
		super("GrabWord"+wordNum);
		List<ISlot> goalprecond = new ArrayList<ISlot>();
		String wordVar = "word";
		
		//goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.Free)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.WordSem, wordNum, maxwords), new SlotVar(wordVar)));
		this.requireSentence(goalprecond);
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		
		List<ISlot> goalmod = new ArrayList<ISlot>();
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.WordSem, wordNum, maxwords), new NullValue()));
		goalmod.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, wordNum, maxwords), new SlotVar(wordVar)));
		//goalmod.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.RetrievedLexSyn)));
		
		List<ISlot> ret = new ArrayList<ISlot>();
		ret.add(new Slot(new LSSlotName(LSSlotNameEnum.Word), new SlotVar(wordVar)));
		
		this.modifyGoal(goalmod);
		this.makeRetrieval(ChunkTypeEnum.lexsyn, ret);
	}

}
