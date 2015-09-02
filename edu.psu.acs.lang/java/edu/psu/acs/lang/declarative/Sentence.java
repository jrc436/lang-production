package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

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
	public Sentence makeSentence(String name, Set<String> wordBag, List<LexSyn> sentence) {
		List<Slot> slots = new ArrayList<Slot>();
		if (wordBag.size() != sentence.size()) {
			System.err.println("The wordbag, in it spresent form, should always be the same size as the sentence");
		}
		int i = 0;
		for (String s : wordBag) {
			slots.add(new Slot("word"+i, new StringValue(s)));
			i++;
		}
		i = 0;
		for (LexSyn l : sentence) {
			slots.add(new Slot("lexsyn"+i, l));
			i++;
		}
		return new Sentence(name, ChunkType.sentence, slots);
	}
	protected Sentence(String name, ChunkType chunkType, List<Slot> slots) {
		super(name, chunkType, slots);
		// TODO Auto-generated constructor stub
	}

}
