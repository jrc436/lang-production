package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;

public class Sentence extends ChunkStore {
	/**
	 * There is some natural confusion here. The Set of Strings can be thought of as a proxy to the semantics. This can eventually be updated
	 * with a better proxy. They will be used as retrieval cues for LexSyns. The List of lexSyns on the other hand is the actual desired output
	 * of the sentence. 
	 * @param name
	 * @param wordBag
	 * @param sentence
	 * @return
	 */
	private final String name;
	public static Sentence makeSentence(String name, List<String> wordBag, int maxtypes, int maxNumWords) {
		List<Slot> slots = new ArrayList<Slot>();
//		slots.add(new Slot(new SSlotName(SSlotNameEnum.FullType), new NullValue()));
//		slots.add(new Slot(new SSlotName(SSlotNameEnum.LeftFullType), new NullValue()));
//		slots.add(new Slot(new SSlotName(SSlotNameEnum.RightFullType), new NullValue()));
//		slots.add(new Slot(new SSlotName(SSlotNameEnum.Combinator), new NullValue()));
		for (int i = 1; i <= maxNumWords; i++) {
			int k = i-1;
			if (k < wordBag.size()) {
				slots.add(new Slot(new SSlotName(SSlotNameEnum.WordSem, i, maxNumWords), new Word(wordBag.get(k))));
			}
			else {
				//blank out all the ones it doesn't have a word for.
				slots.add(new Slot(new SSlotName(SSlotNameEnum.WordSem, i, maxNumWords), new NullValue()));
			}
			slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, i, maxNumWords), new NullValue()));
			for (int j = 1; j <= maxtypes; j++) {
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, i, maxNumWords, j, maxtypes), new NullValue()));
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, i, maxNumWords, j, maxtypes), new NullValue()));
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, i, maxNumWords, j, maxtypes), new NullValue()));
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, i, maxNumWords, j, maxtypes), new NullValue()));
				slots.add(new Slot(new SSlotName(SSlotNameEnum.LexsynConj, i, maxNumWords, j, maxtypes), new NullValue()));
			}		
		}
		return new Sentence(name, ChunkTypeEnum.sentence, slots);
	}
	protected Sentence(String name, ChunkTypeEnum chunkType, List<Slot> slots) {
		super(chunkType, slots);
		this.name = name;
	}
	public String toString() {
		return name;
	}

}
