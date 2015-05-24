package grammar;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import unify.SimpleType;


//note that due to the importance of typesdata in initial construction, a temporary typesdata will be created during the reading.
//future typesdata will be based off of it.
public class TypesData {
	 private final Map<String,SimpleType> nameToType;
	 private final List<SimpleType> indexToType;
	 
	 public int size() {
		 return indexToType.size();
	 }
	 public TypesData(Types t) {
		 List<SimpleType> copylist = t.copyList();
		 nameToType = new LinkedHashMap<String, SimpleType>();
		 indexToType = new ArrayList<SimpleType>();
		 for (SimpleType type : copylist) {
			 indexToType.add(type);
			 nameToType.put(type.getName(), type);	
		 }
	 }
	 public TypesData(TypesData typ) {
		 nameToType = new LinkedHashMap<String, SimpleType>();
		 indexToType = new ArrayList<SimpleType>();
		 for (int i = 0; i < typ.indexToType.size(); i++) {
			 indexToType.add(typ.indexToType.get(i));
			 nameToType.put(typ.indexToType.get(i).getName(), typ.indexToType.get(i));
		 }
	 }
	 
	  /** Returns the simple type with the given name, or a new one if none yet exists. */
    public SimpleType getSimpleType(String typeName) {
        SimpleType type = nameToType.get(typeName);
        if (type == null) {
            BitSet bs = new BitSet();
            bs.set(indexToType.size());
            SimpleType newtype = new SimpleType(indexToType.size(), typeName, bs);
            setTopBitSet(indexToType.size());
           // System.err.println();
            
            nameToType.put(typeName, newtype);
            indexToType.add(newtype);   
            type = newtype;
        }
      //  System.exit(1);
        return type;
    }
	 public SimpleType getAtIndex(int i) {
		if (i >= indexToType.size()) {
			System.err.println("out of bounds"+i);
			System.exit(1);
		}
    	return indexToType.get(i);
    }
    /** Returns whether there is a simple type with the given name. */
    public boolean containsSimpleType(String typeName) {
        return nameToType.containsKey(typeName);
    }
    private void setTopBitSet(int index) {
    	SimpleType top = nameToType.get(Types.TOP_TYPE);
    	SimpleType newTop = top.setbit(index);
    	indexToType.set(top.getIndex(), newTop);
    	nameToType.put(top.getName(), newTop);
    	
    }
}
