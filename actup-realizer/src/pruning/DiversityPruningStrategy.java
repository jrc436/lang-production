///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2004-5 University of Edinburgh (Michael White)
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
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import realize.Edge;
import runconfig.Consts;
import synsem.Sign;

/**
 * Abstract n-best edge pruning strategy that keeps edges diversified 
 * according to the notCompellinglyDifferent equivalence relation. 
 * The edges are clustered into a ranked list of equivalence classes, 
 * which are sequentially sampled until the limit n is reached to determine 
 * the edges to keep.
 * If the singleBestPerGroup flag is set, then a maximum of one edge 
 * per equivalence class is retained.
 *
 * @author      Michael White
 * @version     $Revision: 1.4 $, $Date: 2009/12/21 03:27:18 $
 */
abstract public class DiversityPruningStrategy extends NBestPruningStrategy
{
    public DiversityPruningStrategy(int pruningVal) {
		super(pruningVal);
	}

	/** Flag for whether to keep only the single best edge among those that 
        are not compellingly different (defaults to false). */
    public boolean singleBestPerGroup = false;
    
    /** Reusable set of edges to keep. */
    protected Set<Edge> keepers = new LinkedHashSet<Edge>();
    
    /** Returns true iff the given signs are not compellingly different. */
    abstract public boolean notCompellinglyDifferent(Sign sign1, Sign sign2);
    
    /**
     * Returns a (possibly empty) list of edges pruned 
     * from the given ones, which should have equivalent
     * categories and be sorted by score, from highest to lowest. 
     * In particular, prunes and returns the edges that fall below the N-best 
     * cutoff when the diversity strategy determined by notCompellinglyDifferent 
     * is applied.
     * If the singleBestPerGroup flag is set, no more than one edge 
     * per group of equivalent ones will be returned.
     */
    public List<Edge> pruneEdges(List<Edge> catEdges) {
        // clear reusable return list
        List<Edge> retval = new ArrayList<Edge>();
        // ensure pruning enabled
        if (pruneCutoff == Consts.PRUNE_LEAST_EDGES) {
        	return retval;
        }
        // ensure there are edges to prune
        if (!singleBestPerGroup && catEdges.size() <= pruneCutoff) return retval;
        // group edges into ranked equivalence classes, 
        // by using a list of lists, preserving order
        List<List<Edge>> groups = new ArrayList<List<Edge>>();
        for (Edge edge : catEdges) {
            boolean foundGroup = false;
            for (int i = 0; i < groups.size(); i++) {
                List<Edge> members = groups.get(i);
                Edge first = members.get(0);
                if (notCompellinglyDifferent(first.getSign(), edge.getSign())) {
                    members.add(edge); 
                    foundGroup = true; break;
                }
            }
            if (!foundGroup) {
                List<Edge> members = new ArrayList<Edge>();
                members.add(edge);
                groups.add(members);
            }
        }
        // add top n to keepers by sequentially visiting groups, 
        // according also to singleBestPerGroup flag
        keepers.clear();
        int counter = 0;
        int numGroups = groups.size();
        while (keepers.size() < pruneCutoff && 
               (!singleBestPerGroup || counter < numGroups)) 
        {
            int groupNum = counter % numGroups;
            int indexInGroup = counter / numGroups;
            List<Edge> members = groups.get(groupNum);
            if (indexInGroup < members.size()) { 
                keepers.add(members.get(indexInGroup));
            }
            counter++;
        }
        // prune edges not in keepers
        for (Iterator<Edge> it = catEdges.iterator(); it.hasNext(); ) {
            Edge edge = it.next();
            if (!keepers.contains(edge)) {
                retval.add(edge);
                it.remove();
            }
        }
        // done
        return retval;
    }
}

