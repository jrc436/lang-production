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

public class AddLexSyn extends ProductionRule {

	public AddLexSyn(int memNum, int maxNumTypes, int wmSize) {
		super("addlexsyn"+memNum);
		
		String wordVar = "word";
		
		//we actually need ModelCreator.MaxNum here.
		String[] typeVar = new String[maxNumTypes];
		String[] leftTypeVar = new String[maxNumTypes];
		String[] rightTypeVar = new String[maxNumTypes];
		String[] combos = new String[maxNumTypes];
		String[] conjs = new String[maxNumTypes];
		for (int i = 0; i < maxNumTypes; i++) {
			typeVar[i] = "type"+i;
			leftTypeVar[i] = "ltype"+i;
			rightTypeVar[i] = "rtype"+i;
			combos[i] = "combo"+i;
			conjs[i] = "conj"+i;
		}
		
		List<ISlot> goalprecond = new ArrayList<ISlot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, memNum, wmSize), new SlotVar(wordVar)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, memNum, wmSize, 1, maxNumTypes), new NullValue())); //check if any types are populated
		//goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.RetrievedLexSyn)));
		requireSentence(goalprecond);
		
		//the trick here is we need to populate the cue with *all* of the types....
		//if some of the types are empty, they still need to be copied. 
		List<ISlot> retcheck = new ArrayList<ISlot>();
		retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.Word), new SlotVar(wordVar)));
		for (int i = 1; i <= maxNumTypes; i++) {
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.Type, i, maxNumTypes), new SlotVar(typeVar[i-1])));
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.LeftType, i, maxNumTypes), new SlotVar(leftTypeVar[i-1])));
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.RightType, i, maxNumTypes), new SlotVar(rightTypeVar[i-1])));
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.Combinator, i, maxNumTypes), new SlotVar(combos[i-1])));
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.Conj, i, maxNumTypes), new SlotVar(conjs[i-1])));
		}
		requireRetrieval(retcheck, ChunkTypeEnum.lexsyn);
		
		List<ISlot> cueAdds = new ArrayList<ISlot>();
		for (int i = 1; i <= maxNumTypes; i++) {
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, memNum, wmSize, i, maxNumTypes), new SlotVar(typeVar[i-1])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, memNum, wmSize, i, maxNumTypes), new SlotVar(leftTypeVar[i-1])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, memNum, wmSize, i, maxNumTypes), new SlotVar(rightTypeVar[i-1])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, memNum, wmSize, i, maxNumTypes), new SlotVar(combos[i-1])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, memNum, wmSize, i, maxNumTypes), new SlotVar(conjs[i-1])));
		}
		//cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.Free)));
		modifyGoal(cueAdds);
	}

}
