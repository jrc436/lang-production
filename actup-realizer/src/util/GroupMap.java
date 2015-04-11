///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-6 Jason Baldridge, Gann Bierner and 
//                      Michael White (University of Edinburgh, The Ohio State University)
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//////////////////////////////////////////////////////////////////////////////

package util;

import java.io.Serializable;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * A map where putting a value does not replace an old value but is rather
 * included in a set of values for that key.
 * The map may use identity equals on keys. 
 *
 * @author      Jason Baldridge
 * @author      Gann Bierner
 * @author      Michael White
 * @version     $Revision: 1.9 $, $Date: 2009/07/17 04:23:30 $
 */
public class GroupMap<KeyType, ValType> implements Serializable {
    
	private static final long serialVersionUID = -2995356057195571222L;
	
	// the underlying map
	private Map<KeyType, Set<ValType>> map;
	
	/** Constructor with flag for whether to use identity instead of <code>equals</code> on keys. */
	public GroupMap() {
		map = new LinkedHashMap<KeyType, Set<ValType>>();
	}
	
	public Set<ValType> put(KeyType key, ValType value) {
        Object currentVal = map.get(key);
        if (currentVal == null) {
        	map.put(key, new LinkedHashSet<ValType>());
        }
        map.get(key).add(value);
        return map.get(key);
    }
	public Set<ValType> get(KeyType key) {
		return map.get(key);
	}
    
    /** Adds a key-value pair to the map for all the given vals. */
    public void putAll(KeyType key, Collection<ValType> vals) {
    	for (ValType val : vals) {
    		put(key, val);
    	}
    }
    public int size() { 
    	return map.size();
    }

    /** Returns the keys. */
	public Set<KeyType> keySet() {
    	return map.keySet();
    }
    
    /** Returns whether the keys contain the given one. */
    public boolean containsKey(KeyType key) {
    	return map.containsKey(key);
    }
    
    /** Removes the given key, returning its previous value (if any). */
    public Set<ValType> remove(KeyType key) {
    	Set<ValType> retval = map.get(key);
    	map.remove(key);
    	return retval;
    }
}
