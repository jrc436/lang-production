///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003 Jason Baldridge and University of Edinburgh (Michael White)
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

package unify;

import java.util.LinkedHashMap;
import java.util.Map;

import synsem.Category;
import synsem.CategoryFcn;
import synsem.CategoryFcnAdapter;
import synsem.LF;

/**
 * Center of command for the unification process.  
 * Needs work to allow for multithreading. 
 *
 * @author      Jason Baldridge
 * @author      Michael White
 * @version     $Revision: 1.6 $, $Date: 2005/10/20 17:30:30 $
 */
public class UnifyControl { 

    /**
     * An integer used to keep variables unique in lexical items.
     */
    private int _varIndex = 0;

    /**
     * A function that makes variables unique.
     */
    private final ModFcn uniqueFcn = new ModFcn() {
        public void modify (Mutable m) {
            if (m instanceof Indexed && m instanceof Variable) {
                ((Indexed)m).setIndex(_varIndex);
            }
        }};


    /**
     * An integer used to keep feature structure indexes unique.
     */
    private int _fsIndex = 1;
    private Map<Integer, Integer> _reindexed = new LinkedHashMap<Integer, Integer>();

    private CategoryFcn indexFcn = new CategoryFcnAdapter() {
        public void forall(Category c) {
            FeatureStructure fs = c.getFeatureStructure();
            if (fs != null) {
                int index = fs.getIndex();
                if (index > 0) {
                    Integer $index = _reindexed.get(index);
                    if ($index == null) { 
                        $index = _fsIndex++;
                        _reindexed.put(index, $index); 
                    }
                    fs.setIndex($index);
                }
            }
        }
    };

    /** Resets the uniqueness counters. */
    public void startUnifySequence() {
        _varIndex = 0;
        _fsIndex = 1;
    }
    
    /** Sets the var and feature structure indices to unique values. */
    public void reindex(Category cat) { 
        reindex(cat, null); 
    }

    /** Sets the var and feature structure indices to unique values. */
    public void reindex(Category cat, Category anotherCat) {
        _reindexed.clear();
        cat.forall(indexFcn);
        cat.deepMap(uniqueFcn);
        if (cat != anotherCat && anotherCat != null) {
            anotherCat.forall(indexFcn);
            anotherCat.deepMap(uniqueFcn);
        }
        _varIndex++;
    }

    public int getUniqueVarIndex() {
        return ++_varIndex;
    }

    public int getUniqueFeatureStructureIndex() {
        return ++_fsIndex;
    }
    
    public Object copy(Object o) {
        if (o instanceof Category) {
            return ((Category)o).copy();
        } else if (o instanceof GFeatVar) {
            return ((GFeatVar)o).copy();
        } else if (o instanceof LF) {
            return ((LF)o).copy();
        } else if (o instanceof GFeatStruc) {
            return ((GFeatStruc)o).copy();
        } else {
            return o;
        }
    }
}

