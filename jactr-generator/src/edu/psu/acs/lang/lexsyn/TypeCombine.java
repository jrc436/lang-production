//package edu.psu.acs.lang.lexsyn;
//
//import java.util.HashMap;
//import java.util.Map;
//
//import edu.psu.acs.lang.declarative.CCGType;
//import util.wordmap.Combinable;
//import util.wordmap.CombineException;
//
//public class TypeCombine extends Combinable  {
//	public static final String betweenWords = ":-:";
//	public static final String wordInt = "<>";
//	private final Map<CCGType, Integer> types;
//	public TypeCombine() {
//		types = new HashMap<CCGType, Integer>();
//	}	
//	protected TypeCombine(Map<CCGType, Integer> m) {
//		this.types = m;
//	}
//	public TypeCombine(TypeCombine tc) {
//		this.types = new HashMap<CCGType, Integer>(tc.types);
//	}
//	public String toString() {
//		String retval = "";
//		for (CCGType c : types.keySet()) {
//			retval += c.toString()+wordInt+types.get(c)+betweenWords;
//		}
//		if (retval.length() == 0) {
//			return "";
//		}
//		return retval.substring(0, retval.length()-betweenWords.length());
//	}
//	@Override
//	public Combinable combine(Combinable other) throws CombineException {
//		if (!(other instanceof TypeCombine)) {
//			throw new CombineException();
//		}
//		TypeCombine olc = (TypeCombine) other;
//		TypeCombine toret = new TypeCombine(olc);
//		for (CCGType t : types.keySet()) {
//			if (toret.types.containsKey(t)) {
//				toret.types.put(t, toret.types.get(t)+types.get(t));
//			}
//			else {
//				toret.types.put(t, types.get(t));
//			}
//		}
//		return toret;
//	}
//
//}
