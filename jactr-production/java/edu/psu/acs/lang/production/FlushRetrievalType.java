package edu.psu.acs.lang.production;

import java.util.ArrayList;

import edu.psu.acs.lang.declarative.chunk.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.slot.ISlot;

public class FlushRetrievalType extends ProductionRule {
	public FlushRetrievalType() {
		super("FlushRetrieval-Type");
		this.makeQuery(BufferQueries.checkGoalEmpty());
		this.requireRetrieval(new ArrayList<ISlot>(), ChunkTypeEnum.CCGType);
		this.flushRetrieval();
		//changeGoal(sv.toString(), new ArrayList<ISlot>());
	}
}
