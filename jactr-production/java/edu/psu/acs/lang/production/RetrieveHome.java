package edu.psu.acs.lang.production;

import java.util.ArrayList;

import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;

public class RetrieveHome extends ProductionRule {
	public static final String sentenceDelimiter = "SENTENCE-END";
	public RetrieveHome() {
		super("RetrieveHome");
		this.makeQuery(BufferQueries.checkGoalEmpty());
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		//refocus goal on the manager
		this.addOutput(sentenceDelimiter);
		this.makeRetrieval(ChunkTypeEnum.sentenceManager, new ArrayList<ISlot>());
	}
//	private class StringValue implements SlotValue {
//		private final String s;
//		private StringValue(String s) {
//			this.s = s;
//		}
//		public String toString() {
//			return s;
//		}
//	}
//	public RetrieveHome(int whichHome) {
//		super("RetrieveHome"+whichHome);
//		this.makeQuery(BufferQueries.checkGoalEmpty());
//		this.makeQuery(BufferQueries.checkRetrievalEmpty());
//		//refocus goal on the manager
//		this.addOutput(sentenceDelimiter);
//		//SlotVar sv1 = new SlotVar("newgoal");
//		List<ISlot> slots = new ArrayList<ISlot>();
//		slots.add(new Slot(new SingletonSlotName(SingletonSlotNameEnum.goal), new StringValue(Sentence.getNameConst(whichHome))));
//		this.makeRetrieval(ChunkTypeEnum.sentenceManager, slots);
//	}
//	private class StringValue implements SlotValue {
//		private final String s;
//		private StringValue(String s) {
//			this.s = s;
//		}
//		public String toString() {
//			return s;
//		}
//	}

}
