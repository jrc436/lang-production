package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;

public class SentenceManager extends ChunkStore {
	private static final String managerName = "monologue";
	private final int num;
	protected SentenceManager(int sentenceNum, List<Slot> slots) {
		super(ChunkTypeEnum.sentenceManager, slots);
		this.num = sentenceNum;
	}
	public static final SentenceManager makeSentenceManager(int sentNum, Sentence sentence) {
		List<Slot> slots = new ArrayList<Slot>();
		slots.add(new Slot(new SingletonSlotName(SingletonSlotNameEnum.goal), sentence));		
		return new SentenceManager(sentNum, slots);
	}
	public String toString() {
		return managerName + num;
	}
	public static String getFirst() {
		return managerName + 1;
	}
}
