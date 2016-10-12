package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.chunk.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.slot.BooleanSlotVal;
import edu.psu.acs.lang.declarative.slot.ISlot;
import edu.psu.acs.lang.declarative.slot.MetaSlot;
import edu.psu.acs.lang.declarative.slot.MetaSlotEnum;
import edu.psu.acs.lang.settings.ExperimentSettings;

public class RetrieveHome extends ProductionRule {
	
	public RetrieveHome() {
		super("RetrieveHome");
		this.makeQuery(BufferQueries.checkGoalEmpty());
		this.makeQuery(BufferQueries.checkRetrievalFree());
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		this.makeQuery(BufferQueries.checkGoalFree());
		//refocus goal on the manager
		this.addOutput(ExperimentSettings.sentenceDelimiter);
		List<ISlot> slots = new ArrayList<ISlot>();
		//slots.add(new Slot(new SingletonSlotName(SingletonSlotNameEnum.goal), new NotNullValue()));
		slots.add(new MetaSlot(MetaSlotEnum.recentlyRetrieved, BooleanSlotVal.False));
		this.makeRetrieval(ChunkTypeEnum.sentence, slots);
	}

}
