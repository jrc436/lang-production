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
	public static LexSyn makeLexSyn(String word, List<CCGType> potentialSlots, int maxtypes) {
		List<Slot> slots = new ArrayList<Slot>();
		slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Word), new StringValue(word)));
		//just add a base type it can fulfill, a forward slot, and a backward slot, if any!
		for (int j = 1; j <= maxtypes; j++) {
			int i = j - 1; //list indexing is 0'd, type indexing is 1 (declared by me... painfully)
			if (i < potentialSlots.size()) {
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Type, j), potentialSlots.get(i)));
				if (potentialSlots.get(i) instanceof CCGCompoundType) {
					CCGCompoundType t = (CCGCompoundType) potentialSlots.get(i);
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.LeftType, j), t.getLeft()));
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.RightType, j), t.getRight()));
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Combinator, j), t.getOperator()));
				}
				else {
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.LeftType, j), new NullValue()));
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.RightType, j), new NullValue()));
					slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Combinator, j), new NullValue()));
				}
			}
			//blank out all of the ones it doesn't have types for...
			else {
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Type, j), new NullValue()));
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.LeftType, j), new NullValue()));
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.RightType, j), new NullValue()));
				slots.add(new Slot(new LSSlotName(LSSlotNameEnum.Combinator, j), new NullValue()));
			}
		}
		return new LexSyn(word+"-lexchunk", ChunkTypeEnum.lexsyn, slots);
	}
	protected LexSyn(String name, ChunkTypeEnum chunkType, List<Slot> slots) {
		super(name, chunkType, slots);
	}

}
