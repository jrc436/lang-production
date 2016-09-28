package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.SlotVar;
import edu.psu.acs.lang.settings.ExperimentSettings;

public class FocusHome extends ProductionRule {
	
	public FocusHome() {
		super("FocusHome");
		this.makeQuery(BufferQueries.checkGoalEmpty());
		//SlotVar sv1 = new SlotVar("newgoal");
		List<ISlot> slots = new ArrayList<ISlot>();
		//slots.add(new Slot(new SingletonSlotName(SingletonSlotNameEnum.goal), new NotNullValue()));
		this.requireRetrieval(slots, ChunkTypeEnum.sentence); //will only focus a goal with its goal not dead
		SlotVar sv = new SlotVar("retrieval");
		changeGoal(sv.toString(), new ArrayList<ISlot>(), true);
		this.addOutput(ExperimentSettings.newGoal+": "+sv.toString());
	}
}
