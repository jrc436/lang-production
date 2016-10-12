package edu.psu.acs.lang.declarative.type;

import java.util.ArrayList;
import java.util.Arrays;

import edu.psu.acs.lang.declarative.chunk.ChunkStore;
import edu.psu.acs.lang.declarative.chunk.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.chunk.EmptyChunk;
import edu.psu.acs.lang.declarative.chunk.EmptyEnum;
import edu.psu.acs.lang.declarative.slot.NullValue;
import edu.psu.acs.lang.declarative.slot.Slot;

public abstract class CCGType extends ChunkStore {
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
	protected CCGType(boolean nil) {
		super(ChunkTypeEnum.CCGType, new ArrayList<Slot>(Arrays.asList(new Slot[]{ 
				new Slot(CCGTypeSlot.LeftType, new NullValue()),
				new Slot(CCGTypeSlot.RightType, new NullValue()),
				new Slot(CCGTypeSlot.Combinator, new NullValue()),
		})));
	}
	public static CCGType factory() {
		CCGBaseType ct = new CCGBaseType(null, null);
		ct.addSlot(CCGTypeSlot.FullType, new NullValue());
		return ct;
	}
//	protected abstract void makeConjable();
//	protected abstract void makeUnconjable();
//	protected abstract void purifyConj(); //this will remove conjable from any non parent types (which should theoretically just be its right-most leaf)
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
	public boolean isConjugation() {
		if (!(this instanceof CCGBaseType)) {
			return false;
		}
		CCGBaseType cbt = (CCGBaseType) this;
		switch (cbt.getTypeEnum()) {
			case conj:
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
