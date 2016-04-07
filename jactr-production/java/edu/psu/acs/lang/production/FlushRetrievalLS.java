package edu.psu.acs.lang.production;

import java.util.ArrayList;

import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;

public class FlushRetrievalLS extends ProductionRule {
	public FlushRetrievalLS() {
		super("FlushRetrieval-LS");
		this.makeQuery(BufferQueries.checkGoalEmpty());
		this.requireRetrieval(new ArrayList<ISlot>(), ChunkTypeEnum.lexsyn);
		this.flushRetrieval();
		//changeGoal(sv.toString(), new ArrayList<ISlot>());
	}
}
