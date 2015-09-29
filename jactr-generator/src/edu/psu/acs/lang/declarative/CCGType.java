package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.Arrays;

public class CCGType extends ChunkStore implements SlotValue {
	private static int count = 0; //kind of dumb. oh well.
	protected CCGType(CCGType leftSlot, CCGType rightSlot, CCGOperator combo) {
		super("type"+count, ChunkTypeEnum.CCGType, new ArrayList<Slot>(Arrays.asList(new Slot[]{ 
				new Slot(CCGTypeSlot.LeftType, leftSlot),
				new Slot(CCGTypeSlot.RightType, rightSlot),
				new Slot(CCGTypeSlot.Combinator, combo),
		})));
		count++;
	}
	protected CCGType() {
		super("type"+count, ChunkTypeEnum.CCGType, new ArrayList<Slot>(Arrays.asList(new Slot[]{ 
				new Slot(CCGTypeSlot.LeftType, new NullValue()),
				new Slot(CCGTypeSlot.RightType, new NullValue()),
				new Slot(CCGTypeSlot.Combinator, new NullValue()),
		})));
		count++;
	}
}
