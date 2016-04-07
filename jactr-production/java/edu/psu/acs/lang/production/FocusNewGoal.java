package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SingletonSlotName;
import edu.psu.acs.lang.declarative.SingletonSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class FocusNewGoal extends ProductionRule {

	public FocusNewGoal() {
		super("FocusSentence");
		//first check if the goal is a SentenceManager
		//if it is, then make sure this sentence isn't null
		SlotVar sv = new SlotVar("newgoal");
		//String chunkName = Sentence.getNameConst(goalNum);
		List<ISlot> slots = new ArrayList<ISlot>();
		slots.add(new Slot(new SingletonSlotName(SingletonSlotNameEnum.goal), sv));
		super.ensureSentenceManager(slots);
		
		//if it's not, then set the goal to this sentence, and make this sentence in the old goal null
		List<ISlot> oslots = new ArrayList<ISlot>();
		oslots.add(new Slot(new SingletonSlotName(SingletonSlotNameEnum.goal), new NullValue()));
		super.changeGoal(sv.toString(), oslots, false); //not quite sure if this will work right!!
	}

}
