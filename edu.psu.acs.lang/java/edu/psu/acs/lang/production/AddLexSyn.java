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

public class AddLexSyn extends ProductionRule {

	public AddLexSyn(int wordNum, int maxNumTypes, int maxNumWords) {
		super("addlexsyn"+wordNum);
		
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
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, wordNum, maxNumWords), new SlotVar(wordVar)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, wordNum, maxNumWords, 1, maxNumTypes), new NullValue())); //check if any types are populated
		//goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.RetrievedLexSyn)));
		requireGoal(goalprecond);
		
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
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, wordNum, maxNumWords, i, maxNumTypes), new SlotVar(typeVar[i-1])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, wordNum, maxNumWords, i, maxNumTypes), new SlotVar(leftTypeVar[i-1])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, wordNum, maxNumWords, i, maxNumTypes), new SlotVar(rightTypeVar[i-1])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, wordNum, maxNumWords, i, maxNumTypes), new SlotVar(combos[i-1])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, wordNum, maxNumWords, i, maxNumTypes), new SlotVar(conjs[i-1])));
		}
		//cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.State), new GoalState(GoalStateEnum.Free)));
		modifyGoal(cueAdds);
	}

}
