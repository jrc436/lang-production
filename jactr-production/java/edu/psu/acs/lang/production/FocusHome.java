package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.SingletonSlotName;
import edu.psu.acs.lang.declarative.SingletonSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class FocusHome extends ProductionRule {

	public FocusHome() {
		super("FocusHome");
		this.makeQuery(BufferQueries.checkGoalEmpty());
		SlotVar sv1 = new SlotVar("newgoal");
		List<ISlot> slots = new ArrayList<ISlot>();
		slots.add(new Slot(new SingletonSlotName(SingletonSlotNameEnum.goal), sv1));
		this.requireRetrieval(slots, ChunkTypeEnum.sentenceManager); //will only focus a goal with its goal not dead
		SlotVar sv = new SlotVar("retrieval");
		changeGoal(sv.toString(), new ArrayList<ISlot>(), true);
	}

}
