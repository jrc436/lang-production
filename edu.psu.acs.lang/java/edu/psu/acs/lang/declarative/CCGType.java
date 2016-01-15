package edu.psu.acs.lang.declarative;

import java.util.ArrayList;
import java.util.Arrays;

public abstract class CCGType extends ChunkStore implements SlotValue {
	protected CCGType(CCGType leftSlot, CCGType rightSlot, CCGOperator combo) {
		super(ChunkTypeEnum.CCGType, new ArrayList<Slot>(Arrays.asList(new Slot[]{ 
				new Slot(CCGTypeSlot.LeftType, leftSlot),
				new Slot(CCGTypeSlot.RightType, rightSlot),
				new Slot(CCGTypeSlot.Combinator, combo),
		})));
	}
	protected CCGType() {
		super(ChunkTypeEnum.CCGType, new ArrayList<Slot>(Arrays.asList(new Slot[]{ 
				new Slot(CCGTypeSlot.LeftType, new EmptyChunk(EmptyEnum.NA)),
				new Slot(CCGTypeSlot.RightType, new EmptyChunk(EmptyEnum.NA)),
				new Slot(CCGTypeSlot.Combinator, new EmptyChunk(EmptyEnum.NA)),
		})));
	}
	public abstract boolean isConjable();
	public boolean isPCT() {
		if (!(this instanceof CCGBaseType)) {
			return false;
		}
		CCGBaseType cbt = (CCGBaseType) this;
		switch (cbt.getTypeEnum()) {
		case Colon:
			return true;
		case Comma:
			return true;
		case Period:
			return true;
		default:
			return false;		
		}
		
	}
	@Override
	public boolean equals(Object other) {
		if (other == null || !(other instanceof CCGType)) {
			return false;
		}
		return other.toString().equals(this.toString());
	}
	@Override
	public int hashCode() {
		return this.toString().hashCode();
	}
	public abstract CCGType getRight();
	public abstract CCGType getLeft();
	public abstract CCGOperator getCombo();
}
