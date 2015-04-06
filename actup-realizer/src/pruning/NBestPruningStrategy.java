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

package pruning;

import java.util.ArrayList;
import java.util.List;

import realize.Edge;

/**
 * Default, n-best edge pruning strategy.
 *
 * @author      Michael White
 * @version     $Revision: 1.9 $, $Date: 2011/03/27 14:45:32 $
 */
public class NBestPruningStrategy implements PruningStrategy
{
    /** The current pruning val. */
    protected int pruneCutoff;
    
    /** Reusable return list. */
    protected List<Edge> retval = new ArrayList<Edge>();
    
    /** Constructor with pruning val. */
    public NBestPruningStrategy(int pruningVal) {
        pruneCutoff = pruningVal;
    }
    
    /**
     * Returns a (possibly empty) list of edges pruned 
     * from the given ones, which should be sorted by score, 
     * from highest to lowest. 
     * In particular, prunes and returns the edges that follow the N-best 
     * ones in the given list.
     */
    public List<Edge> pruneEdges(List<Edge> catEdges) {
        retval.clear();
        while (pruneCutoff < catEdges.size()) {
            retval.add(catEdges.remove(pruneCutoff));
        }
        return retval;
    }
}

