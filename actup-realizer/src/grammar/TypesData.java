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
	 private int maxindex;

	 
	 public int size() {
		 return indexToType.size();
	 }
	 public TypesData(Types t) {
		 List<SimpleType> copylist = t.copyList();
		 nameToType = new LinkedHashMap<String, SimpleType>();
		 indexToType = new ArrayList<SimpleType>();
		 for (SimpleType type : copylist) {
			 type.setTypesData(this);
			 indexToType.add(type);
			 nameToType.put(type.getName(), type);	
			 if (type.bitset.get(indexToType.size())) {
	            	System.err.println("hi from types constructor");
	            	System.exit(1);
	         }
		 }
		 maxindex = copylist.size();
	 }
	 public TypesData(TypesData typ) {
		 nameToType = new LinkedHashMap<String, SimpleType>();
		 indexToType = new ArrayList<SimpleType>();
		 for (int i = 0; i < typ.indexToType.size(); i++) {
			 indexToType.add(typ.indexToType.get(i));
			 nameToType.put(typ.indexToType.get(i).getName(), typ.indexToType.get(i));
			 if (indexToType.get(i).getIndex() != i || i != typ.indexToType.get(i).getIndex()) {
				 System.err.println("Indexes are wrong");
				 System.exit(1);
			 }
		 }
		 for (SimpleType st : indexToType) {
			 if (st.bitset.get(indexToType.size())) {
				 	System.err.println("hi from typesdata contsructor");
				 	System.err.println(st.getIndex());
				 	System.err.println(indexToType.size());
	            	System.exit(1);
			 }
		 }
		 maxindex = typ.maxindex;
	 }
	 
	  /** Returns the simple type with the given name, or a new one if none yet exists. */
    public SimpleType getSimpleType(String typeName) {
    	 for (SimpleType st : indexToType) {
	            if (st.bitset.get(indexToType.size())) {
	            	System.err.println("hi");
	            	System.exit(1);
	            }
        }
        SimpleType type = nameToType.get(typeName);
        if (type == null) {
            BitSet bs = new BitSet();
            bs.set(maxindex);
            SimpleType newtype = new SimpleType(maxindex, typeName, bs, this);
            setTopBitSet(maxindex);
           // System.err.println();
            
            nameToType.put(typeName, newtype);
            indexToType.add(newtype);   
            
            for (SimpleType st : indexToType) {
	            if (st.bitset.get(indexToType.size())) {
	            	System.err.println("hi from get");
	            	System.exit(1);
	            }
            }
            maxindex += 1;
            
            //note: before this said ++, which I believe occurs AFTER it would perform the method. Since "add" would already update
            //size, I am subtracting 1 from size
            
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
    	top.setBit(index);
    	//SimpleType newTop = top.setBitsetIndex(index);
    	//indexToType.set(top.getIndex(), newTop);
    	//nameToType.put(top.getName(), newTop);
    	
    }
}
