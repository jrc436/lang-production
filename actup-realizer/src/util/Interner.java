///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2004 University of Edinburgh (Michael White)
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

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.WeakHashMap;

/**
 * A utility class for interning (canonicalizing) objects.
 * A WeakHashMap is used as the backing store, so that interned objects can
 * be garbage collected.
 * Normally, it is easier to use the globalIntern method (sharing a global 
 * backing store) than to allocate separate interners.
 * Individual interners can be constructed to use soft references to 
 * the interned objects, so that they are kept around longer than is the 
 * case with weak references (the default). 
 *
 * @author      Michael White
 * @version     $Revision: 1.5 $, $Date: 2005/10/13 20:33:49 $
 *
 */
public class Interner<T> {

    // the backing store
    private Map<T,Reference<T>> weakMap = new WeakHashMap<T,Reference<T>>();
    
    // flag for whether to use soft references 
    private boolean softRefs = false;
    
    /** Default constructor. */
    public Interner() {}

    /** Constructor with soft references flag. */
    public Interner(boolean softRefs) { this.softRefs = softRefs; }

    /** 
     * Returns a canonical version of the given object.
     * The returned object is .equals() to the given one.
     * If the given object is not equal to one already seen, 
     * then the returned object will be == to the given one.
     */
    public T intern(T obj) {
        // check if equivalent key already in map
        if (weakMap.containsKey(obj)) {
            // return existing canonical obj if so 
            Reference<T> ref = weakMap.get(obj);
            return ref.get();
        }
        // otherwise add this object to the map, wrapped in a 
        // weak/soft reference so that it can still be gc'ed
        Reference<T> ref = (softRefs) 
            ? new SoftReference<T>(obj) 
            : new WeakReference<T>(obj);
        weakMap.put(obj, ref);
        return obj;
    }
    
    /** 
     * Returns the canonical version of the given object, if any, 
     * otherwise returns null.
     */
    public T getInterned(T obj) {
        // get weak reference to canonical obj, if any
        Reference<T> ref = weakMap.get(obj);
        // return obj, if any, otherwise null
        return (ref != null) ? ref.get() : null;
    }
    
    /** Returns the number of interned objects. */
    public int size() {
        return weakMap.size();
    }
}
