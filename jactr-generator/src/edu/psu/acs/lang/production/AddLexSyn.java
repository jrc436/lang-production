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

public class AddLexSyn extends ProductionRule {

	public AddLexSyn(int wordNum, int maxNumTypes) {
		super("addlexsyn"+wordNum, new ArrayList<BufferConditions>(), new ArrayList<BufferEffects>(), new ArrayList<String>());
		
		String wordVar = "word";
		
		//we actually need ModelCreator.MaxNum here.
		String[] typeVar = new String[maxNumTypes];
		String[] leftTypeVar = new String[maxNumTypes];
		String[] rightTypeVar = new String[maxNumTypes];
		String[] combos = new String[maxNumTypes];
		for (int i = 0; i < maxNumTypes; i++) {
			typeVar[i] = "type"+i;
			leftTypeVar[i] = "ltype"+i;
			rightTypeVar[i] = "rtype"+i;
			combos[i] = "combo"+i;
		}
		
		List<Slot> goalprecond = new ArrayList<Slot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.Cue, wordNum), new SlotVar(wordVar)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.CueType, wordNum, 1), new NullValue())); //check if any types are populated
		super.conditions.add(new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, goalprecond));
		
		//the trick here is we need to populate the cue with *all* of the types....
		List<Slot> retcheck = new ArrayList<Slot>();
		retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.Word), new SlotVar(wordVar)));
		for (int i = 1; i <= maxNumTypes; i++) {
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.Type, i), new SlotVar(typeVar[i])));
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.LeftType, i), new SlotVar(leftTypeVar[i])));
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.RightType, i), new SlotVar(rightTypeVar[i])));
			retcheck.add(new Slot(new LSSlotName(LSSlotNameEnum.Combinator, i), new SlotVar(combos[i])));
		}
		super.conditions.add(new BufferConditions(Buffer.retrieval, ChunkTypeEnum.lexsyn, retcheck));
		
		List<Slot> cueAdds = new ArrayList<Slot>();
		for (int i = 1; i <= maxNumTypes; i++) {
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.CueType, wordNum, i), new SlotVar(typeVar[i])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.CueLeftType, wordNum, i), new SlotVar(leftTypeVar[i])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.CueRightType, wordNum, i), new SlotVar(rightTypeVar[i])));
			cueAdds.add(new Slot(new SSlotName(SSlotNameEnum.CueCombo, wordNum, i), new SlotVar(combos[i])));
		}
		super.effects.add(BufferEffects.modifyGoal(cueAdds));
	}

}
