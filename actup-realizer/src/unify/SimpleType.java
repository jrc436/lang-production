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
    private final BitSet bitset;
    
    public SimpleType setbit(int index) {
    	BitSet bs = (BitSet) this.bitset.clone();
    	bs.set(index);
    	return new SimpleType(this.index, this.name, bs);
    }
    
    public SimpleType(int i, String n, BitSet bs) {
    	this.index = i;
    	this.name = n;
    	this.bitset = bs;
    }
    public SimpleType(SimpleType st) {
    	this.index = st.index;
    	this.name = st.name;
    	this.bitset = (BitSet) st.bitset.clone();
    }

    public String getName() { return name; }

    public String toString() { return name; }
    
    public int getIndex() { return index; } 
    
    public void unifyCheck(Object u) throws UnifyFailure {
        if (!(u instanceof SimpleType)) {
            throw new UnifyFailure();
        }
    }
    
    //this returns the version of this type used by the given typesdata, rather than any reference to it
    private SimpleType getCanon(TypesData td) {
    	return td.getAtIndex(index);
    }

    public Object unify(Object u, Substitution sub, UnifyControl uc) throws UnifyFailure {
        if (!(u instanceof SimpleType)) {
            throw new UnifyFailure();
        }    
        if (this.equals(u)) return this;
        SimpleType st2 = (SimpleType) u;
        BitSet tempBitset = new BitSet();
        tempBitset.or(this.getCanon(uc.getTD()).bitset);
        tempBitset.and(st2.getCanon(uc.getTD()).bitset);
        int resultTypeIndex = tempBitset.nextSetBit(0);
        if (resultTypeIndex == -1) {
        	throw new UnifyFailure();
        }      
        return uc.getTD().getAtIndex(resultTypeIndex);
    }

    public Object fill(UnifyControl uc, Substitution s) throws UnifyFailure {
        return this.getCanon(uc.getTD());
    }
    
    public boolean occurs(Variable v) { return false; }
    
    public int hashCode() { return index; } 
    
    public boolean equals(Object o) {
        if (!(o instanceof SimpleType)) return false;
        if (index == ((SimpleType)o).index) return true;
        else return false;
    }
}
