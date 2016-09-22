package edu.psu.acs.lang.lexsyn;

import java.util.HashSet;
import java.util.Set;

import edu.psu.acs.lang.declarative.CCGType;
import util.wordmap.Combinable;
import util.wordmap.CombineException;

public class TypeCombine extends Combinable  {
	private final Set<CCGType> types;
	public TypeCombine() {
		types = new HashSet<CCGType>();
	}	
	public TypeCombine(Set<CCGType> t) {
		this.types = t;
	}
	@Override
	public Combinable combine(Combinable other) throws CombineException {
		if (!(other instanceof TypeCombine)) {
			throw new CombineException();
		}
		TypeCombine olc = (TypeCombine) other;
		Set<CCGType> set = new HashSet<CCGType>();
		set.addAll(this.types);
		set.addAll(olc.types);
		return new TypeCombine(set);
	}

}
