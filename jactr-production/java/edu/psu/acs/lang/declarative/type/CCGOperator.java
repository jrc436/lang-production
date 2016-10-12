package edu.psu.acs.lang.declarative.type;

import java.util.ArrayList;

import edu.psu.acs.lang.declarative.chunk.ChunkStore;
import edu.psu.acs.lang.declarative.chunk.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.slot.Slot;

public class CCGOperator extends ChunkStore {
	private final CCGOperatorEnum oper;
	public CCGOperator(CCGOperatorEnum op) {
		super(ChunkTypeEnum.operator, new ArrayList<Slot>());
		oper = op;
	}
	@Override
	public boolean equals(Object other) {
		if (other == null || other.getClass() != this.getClass()) {
			return false;
		}
		return this.oper == ((CCGOperator)other).oper;
	}
	public static CCGOperator factory() {
		return new CCGOperator(null);
	}
	public String toString() {
		switch(oper) {
			case Slash:
				return "-";
			case Backslash:
				return "~";
			}
		return null;
	}
	public CCGOperatorEnum value(String s) {
		if (s.equals("/")) {
			return CCGOperatorEnum.Slash;
		}
		else if (s.equals("\\")) {
			return CCGOperatorEnum.Backslash;
		}
		return CCGOperatorEnum.valueOf(s);
	}
}
