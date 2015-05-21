///////////////////////////////////////////////////////////////////////////////

//// Copyright (C) 2003-9 Gunes Erkan and Michael White
//// 
//// This library is free software; you can redistribute it and/or
//// modify it under the terms of the GNU Lesser General Public
//// License as published by the Free Software Foundation; either
//// version 2.1 of the License, or (at your option) any later version.
//// 
//// This library is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//// GNU Lesser General Public License for more details.
//// 
//// You should have received a copy of the GNU Lesser General Public
//// License along with this program; if not, write to the Free Software
//// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
////////////////////////////////////////////////////////////////////////////////

package unify;

import grammar.TypesData;

import java.io.Serializable;
import java.util.BitSet;

/**
 * A simple type for feature values in CCG categories.
 * 
 * Note that during deserialization, the type is resolved using the current grammar.
 * 
 * @author      Gunes Erkan
 * @author 		Michael White
 * @version     $Revision: 1.8 $, $Date: 2009/07/17 04:23:30 $
 */
public class SimpleType implements Unifiable, Serializable {
    
	private static final long serialVersionUID = 7028285176993549672L;
	
	private final int index;
    private final String name;
    public BitSet bitset;
//    
//    //WARNING: FULL IMPLICATIONS OF MUTABIiLITY OF SIMPLETYPE HAVE NOT BEEN INVESTIGATED
    public void setBit(int index) {
    	
    	bitset.set(index);
    }
    
    private TypesData typesList;
    public void setTypesData(TypesData typesdata) {
    	this.typesList = typesdata;    	//return new SimpleType(index, name, (BitSet) bitset.clone(), typesdata);
    }
    public SimpleType(int i, String n, BitSet bs, TypesData td) {
    	this.index = i;
    	this.name = n;
    	this.bitset = bs;
    	this.typesList = td;
    }

    public SimpleType(int i, String n, BitSet bs) {
        this(i, n, bs, null);
    }
//    public void setBitsetIndex(int i) {
//    //	BitSet bs = (BitSet) bitset.clone();
//    	bitset.set(i);
//    	//return new SimpleType(index, name, bs, typesList);
//    }

    public String getName() { return name; }

    public String toString() { return name; }
    
    public int getIndex() { return index; } 
    
    public void unifyCheck(Object u) throws UnifyFailure {
        if (!(u instanceof SimpleType)) {
            throw new UnifyFailure();
        }
    }

    public Object unify(Object u, Substitution sub, UnifyControl uc) throws UnifyFailure {
        if (!(u instanceof SimpleType)) {
            throw new UnifyFailure();
        }    
        if (this.equals(u)) return this;
        SimpleType st2 = (SimpleType) u;
//        if (st2.typesList != this.typesList) {
//        	System.err.println("Different lists");
//        	System.exit(1);
//        }
//        if (bitset.nextSetBit(0) == 53 || st2.bitset.nextSetBit(0) == 53) {
//        	System.err.println("wat the fuk");
//        	System.exit(1);
//        }
        BitSet tempBitset = new BitSet(Math.max(bitset.size(), st2.bitset.size()));
        tempBitset.clear();
        tempBitset.or(bitset);
        tempBitset.and(st2.bitset);
        int resultTypeIndex = tempBitset.nextSetBit(0);
        if (resultTypeIndex == -1) {
        	throw new UnifyFailure();
        }
//        if (resultTypeIndex == 53) {
//        	System.err.println(bitset.nextSetBit(0));
//        	System.err.println(bitset.get(53));
//        	System.err.println(st2.bitset.get(53));
//        	System.exit(1);
//        }
      //  int resultTypeIndex = Math.min(st2.index, index);
        if (typesList.size() > st2.typesList.size()) {
        	return typesList.getAtIndex(resultTypeIndex);
        }
        else {
        	return st2.typesList.getAtIndex(resultTypeIndex);
        }
    }

    public Object fill(UnifyControl uc, Substitution s) throws UnifyFailure {
        return this;
    }
    
    public boolean occurs(Variable v) { return false; }
    
    public int hashCode() { return index; } 
    
    public boolean equals(Object o) {
        if (!(o instanceof SimpleType)) return false;
        if (index == ((SimpleType)o).index) return true;
        else return false;
    }
}
