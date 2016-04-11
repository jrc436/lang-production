package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.BooleanSlotVal;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.MetaSlot;
import edu.psu.acs.lang.declarative.MetaSlotEnum;

public class RetrieveHome extends ProductionRule {
	public static final String sentenceDelimiter = "SENTENCE-END";
	public RetrieveHome() {
		super("RetrieveHome");
		this.makeQuery(BufferQueries.checkGoalEmpty());
		this.makeQuery(BufferQueries.checkRetrievalFree());
		this.makeQuery(BufferQueries.checkRetrievalEmpty());
		this.makeQuery(BufferQueries.checkGoalFree());
		//refocus goal on the manager
		this.addOutput(sentenceDelimiter);
		List<ISlot> slots = new ArrayList<ISlot>();
		//slots.add(new Slot(new SingletonSlotName(SingletonSlotNameEnum.goal), new NotNullValue()));
		slots.add(new MetaSlot(MetaSlotEnum.recentlyRetrieved, BooleanSlotVal.False));
		this.makeRetrieval(ChunkTypeEnum.sentence, slots);
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
