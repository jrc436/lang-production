package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.List;

/**
 * A LexSyn is a declarative memory chunk that associates words to all of their possible types. It is used to retrieve what types
 * a word can take from its semantics
 * @author jrc
 *
 */

public class LexSyn extends ChunkStore implements SlotValue {
	private final String word;
	public static LexSyn makeLexSyn(String word, List<CCGType> potentialSlots, int maxtypes) {
		List<Slot> slots = new ArrayList<Slot>();
		slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Word), new Word(word)));
		//just add a base type it can fulfill, a forward slot, and a backward slot, if any!
		for (int j = 1; j <= maxtypes; j++) {
			int i = j - 1; //list indexing is 0'd, type indexing is 1 (declared by me... painfully)
			if (i < potentialSlots.size()) {
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Type, j, maxtypes), potentialSlots.get(i)));
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Conj, j, maxtypes), new Conjable(potentialSlots.get(i).isConjable())));
				if (potentialSlots.get(i) instanceof CCGCompoundType) {
					CCGCompoundType t = (CCGCompoundType) potentialSlots.get(i);
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.LeftType, j, maxtypes), t.getLeft()));
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.RightType, j, maxtypes), t.getRight()));
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Combinator, j, maxtypes), t.getCombo()));					
				}
				else {
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.LeftType, j, maxtypes), new EmptyChunk(EmptyEnum.NA)));
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.RightType, j, maxtypes), new EmptyChunk(EmptyEnum.NA)));
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Combinator, j, maxtypes), new EmptyChunk(EmptyEnum.NA)));
				}
			}
			//blank out all of the ones it doesn't have types for...
			else {
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Type, j, maxtypes), new EmptyChunk(EmptyEnum.NV)));
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.LeftType, j, maxtypes), new EmptyChunk(EmptyEnum.NV)));
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.RightType, j, maxtypes), new EmptyChunk(EmptyEnum.NV)));
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Combinator, j, maxtypes), new EmptyChunk(EmptyEnum.NV)));
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Conj, j, maxtypes), new EmptyChunk(EmptyEnum.NV)));
			}
		}
		return new LexSyn(word, ChunkTypeEnum.lexsyn, slots);
	}
	protected LexSyn(String word, ChunkTypeEnum chunkType, List<Slot> slots) {
		super(chunkType, slots);
		this.word = word;
	}
	public static String getNameConst(String word) {
		return word+"-lex";
	}
	public String toString() {
		return getNameConst(this.word);
	}

}
