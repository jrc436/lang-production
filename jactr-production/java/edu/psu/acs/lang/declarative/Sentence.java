package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;

public class Sentence extends ChunkStore implements SlotValue {
	/**
	 * There is some natural confusion here. The Set of Strings can be thought of as a proxy to the semantics. This can eventually be updated
	 * with a better proxy. They will be used as retrieval cues for LexSyns. The List of lexSyns on the other hand is the actual desired output
	 * of the sentence. 
	 * @param name
	 * @param wordBag
	 * @param sentence
	 * @return
	 */
	public static int getWorkingMemorySize() {
		return 5;
	}
	public static String getNameConst(int num) {
		return "sentence"+num;
	}
	private final String name;
	public static Sentence makeSentence(int num, List<String> wordBag, int maxtypes, int maxNumWords) {
		List<Slot> slots = new ArrayList<Slot>();
		for (int i = 1; i <= maxNumWords; i++) {
			int k = i-1;
			if (k < wordBag.size()) {
				slots.add(new Slot(new SSlotName(SSlotNameEnum.WordSem, i, maxNumWords), new Word(wordBag.get(k))));
			}
			else {
				//blank out all the ones it doesn't have a word for.
				slots.add(new Slot(new SSlotName(SSlotNameEnum.WordSem, i, maxNumWords), new NullValue()));
			}			
		}
		for (int i = 1; i <= getWorkingMemorySize(); i++) {
			slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, i, maxNumWords), new NullValue()));
			for (int j = 1; j <= maxtypes; j++) {
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, i, maxNumWords, j, maxtypes), new NullValue()));
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, i, maxNumWords, j, maxtypes), new NullValue()));
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, i, maxNumWords, j, maxtypes), new NullValue()));
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, i, maxNumWords, j, maxtypes), new NullValue()));
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, i, maxNumWords, j, maxtypes), new NullValue()));
			}		
		}
		return new Sentence(num, slots);
	}
	protected Sentence(int num, List<Slot> slots) {
		super(ChunkTypeEnum.sentence, slots);
		this.name = getNameConst(num);
	}
	public String toString() {
		return name;
	}

}
