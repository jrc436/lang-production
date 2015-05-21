///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-11 University of Edinburgh / Michael White
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

package realize;

import grammar.RuleGroupData;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import lexicon.Lexicon;
import lexicon.Tokenizer;
import pruning.PruningStrategy;
import runconfig.RealizationSettings;
import synsem.Category;
import synsem.DerivationHistory;
import synsem.Sign;
import util.Identity;

/**
 * The chart manages the creation of edges.  Newly added edges are kept on an 
 * agenda prior to rule applications.  In the anytime mode, the chart completion 
 * process can be interrupted according to the given parameters and preferences 
 * (see Chart.combine).  In the packing mode, completing the chart produces 
 * a packed representation, which may then be unpacked if the DO_UNPACKING 
 * preference is set.
 *
 * A single chart instance per realization request is assumed.
 *
 * @author      Michael White
 * @version     $Revision: 1.79 $, $Date: 2011/08/28 04:05:39 $
 */
public class Chart
{                 
    private final RealizationSettings rSet;
    private final EdgeFactory edgeFactory;
    private final PruningStrategy pruningStrategy;   
    private final Lexicon l;
    private final RuleGroupData rdg;
    private final Tokenizer t;

    private final List<Edge> agenda = new ArrayList<Edge>(); //edges not yet added
    private final List<Edge> edges = new ArrayList<Edge>(); //representative edges
    private final List<Edge> allEdges = new ArrayList<Edge>(); //all unpruned and unpacked edges
    private final List<Edge> supercededEdgesPendingRemoval = new ArrayList<Edge>(); //edges to be removed because less complex  found
    private final Map<Sign,Edge> signMap = new LinkedHashMap<Sign,Edge>(); //keeps track of the current edge for that sign
    private final Map<EdgeSurfaceWords, Edge> edgeHash = new LinkedHashMap<EdgeSurfaceWords, Edge>(); //all edges that have been encountered
    
    //maps edges to an equivalence class that allows alternative edges to be added. Eventually should probably be
    //replaced with something like a group map
    //private Map<EdgeSemantics, Edge> catMap = new LinkedHashMap<EdgeSemantics, Edge>();
    private Map<EdgeSemantics, Edge> catMap = new LinkedHashMap<EdgeSemantics, Edge>();
    
    // cell map: based on input coverage vectors
    private Map<BitSet,Integer> cellMap = new HashMap<BitSet,Integer>();
    
    // non-empty cells: cells to avoid when gluing fragments
    private Set<BitSet> nonEmptyCells  = null;
    
    // reusable bitset for checking non-empty cells
    private transient BitSet tmpBitSet = new BitSet();
    
    private Edge bestEdge = null; //best edge found so far (always complete if possible)
    private Edge bestJoinedEdge = null; //best edge, counting joined edges   
    private int numEdges = 0; //total number of edges that have been added to agenda
    private int cellMax = 0; //maximum number of edges presently in a cell
   
   
    public Chart(RealizationSettings rs, EdgeFactory edgeFactory, Lexicon l, RuleGroupData rdg, PruningStrategy pruningStrategy, Tokenizer t) {
    	this.l = l;
    	this.rdg = rdg;
    	this.rSet = rs;
        this.edgeFactory = edgeFactory;
        this.pruningStrategy = pruningStrategy;
        this.t = t;
    }
    public void initialize() {
        for (Edge edge : edgeFactory.createInitialEdges())  {
            addEdgeToAgenda(edge);
        }
    }
    
    /** 
     * Reinitializes the agenda for gluing fragments.  
     * A runtime exception is thrown if not in packing mode.
     */
    public void reInitForGluing() {
    	// check packing mode
    	if (!rSet.usingPacking()) {
    		System.err.println("Can't glue fragments unless in packing mode!!");
    		return;
    	}
    	// set flags here and in edge factory
    	edgeFactory.gluingFragments = true;
    	edgeFactory.useIndexing = false;
    	// add opt for uncovered preds, unless already done for relaxed relation matching
    	if (!edgeFactory.useRelaxedRelationMatching) {
    		edgeFactory.addLFOptsForUncoveredPreds();
    	}
    	// add opts for rule instances
    	edgeFactory.addLFOptsForRuleInstances();
    	// record non-empty cells
    	nonEmptyCells = new HashSet<BitSet>(cellMap.keySet());
    	// add edges back to agenda, for possible gluing
    	for (Edge edge : edges) {
    		addEdgeToAgenda(edge);
    	}
    }
    
    
    /** 
     * Adds to the chart by iteratively moving an edge from the agenda to the chart, 
     * creating new edges for the agenda by applying available rules, 
     * while updating the best edge found so far, 
     * until a stopping criterion is reached.
     * The basic stopping criterion is when the agenda becomes empty, and thus the 
     * search is done.
     * Otherwise, the search is stopped either when the edge limit (if any) is reached, 
     * or the time limit (if any) is reached, 
     * or the first complete edge is found (if beyond the edge/time limit, and according to the given flag), 
     * or until the new best time limit (if any; anytime case only) beyond the first 
     * complete realization is exceeded.
     * In the packing case, unpacking is then performed according to the preference setting.
     * In the anytime case, if the collect combos option is set, then the combinatory rules 
     * are only invoked when an edge with a new category is moved to the chart, in which case 
     * any successful combinations are collected in the edge's combos data structure; 
     * if the edge instead has an already seen category, new edges are created as 
     * alternatives to the collected combos in its representative, much as with unpacking.
     */
    public void combine(int iterLim) {
    	int currentIter = 0;
        while (!agenda.isEmpty()) {    
            boolean bestEdgeComplete = bestEdge != null && bestEdge.complete(); 
            if (bestEdgeComplete || numEdges > rSet.getEdgeLimit() || currentIter > iterLim) {
                if (!allEdges.contains(bestEdge)) { 
                	addEdgeToChart(bestEdge); 
                }
                break;
            }
            Edge next = agenda.remove(0);            
            if (!addEdgeToChart(next)) { 
            	continue; //edge was pruned or combined with existing edge
            }           
            doEdgeCombos(next);
            currentIter++;
        }

        // set done packing time
        if (rSet.doingUnpacking()) {
            doUnpacking();
        }
        
        // join best fragments, if nec.
        if (rSet.joiningFragments() && !bestEdge.complete()) {
        	joinBestFragments(); 
        }
    }
    
    // does binary combinations with all edges in the chart and unary ones too; 
    // when collecting combos, invokes the combinatory rules only when nec.; 
    // prunes superceded edges before returning
    private void doEdgeCombos(Edge next) {
    	// skip semantically null edges when gluing fragments
    	if (rSet.gluingFragments() && next.bitset.isEmpty()) {
    		return;
    	}
    	// when collecting combos ...
        if (rSet.collectingCombos()) {
            // existing rep case: just make alt edges from collected combos
            Edge nextRep = catMap.get(next.getSemantics());//catMap.get(next.getSemantics());
            if (next != nextRep) {
                addNewEdges(edgeFactory.createAltEdges(next, nextRep));
                // and prune any superceded edges before returning
                pruneSupercededEdges();
                return;
            }
        }
        // otherwise combine edge with those in chart 
        List<Edge> edgesToUse = (rSet.usingPacking() || rSet.collectingCombos()) ? edges : allEdges;
        for (Edge edge : edgesToUse) {
            if (edge == next) continue; // skip this edge
            // skip fragment gluing if semantically null or if result cell non-empty
            if (rSet.gluingFragments()) {
            	if (edge.bitset.isEmpty()) continue;
            	tmpBitSet.clear();
            	tmpBitSet.or(edge.bitset); tmpBitSet.or(next.bitset);
            	if (nonEmptyCells.contains(tmpBitSet)) continue;
            }
            // add new combos to agenda
            addNewEdges(edgeFactory.createNewEdges(edge, next, rSet.collectingCombos()));
        }
        // combine edge via unary rules and with semantically null edges, 
        // adding new edges to the agenda
    	addNewEdges(edgeFactory.createNewEdges(next, rSet.collectingCombos()));
        // prune any superceded edges before returning
        pruneSupercededEdges();
    }

    // adds all new edges to the agenda
    private void addNewEdges(List<Edge> newEdges) {
        for (Edge newEdge : newEdges) {
            addEdgeToAgenda(newEdge);
        }
    }
    
    /** Greedily combines best fragments, updating bestJoinedEdge. */
    protected void joinBestFragments() {
    	// start with best edge
    	bestJoinedEdge = bestEdge;
    	// greedily find best fragments
    	List<Edge> fragments = new ArrayList<Edge>();
    	BitSet bitset = bestEdge.bitset;
    	while (true) {
        	Edge bestFrag = null;
        	for (Edge edge : allEdges) bestFrag = chooseBestFrag(bitset, bestFrag, edge);
        	for (Edge edge : agenda) bestFrag = chooseBestFrag(bitset, bestFrag, edge);
        	if (bestFrag == null) break;
        	fragments.add(bestFrag);
        	bitset = (BitSet) bitset.clone();
        	bitset.or(bestFrag.bitset);
    	}
    	// greedily join
    	while (fragments.size() > 0) {
    		Edge nextJoinedEdge = null; Edge nextFrag = null;
	    	for (Edge edge : fragments) {
	    		Edge joinedEdge = edgeFactory.makeJoinedEdge(bestJoinedEdge, edge);
	    		if (nextJoinedEdge == null || nextJoinedEdge.score < joinedEdge.score) {
	    			nextJoinedEdge = joinedEdge; nextFrag = edge;
	    		}
	    		Edge joinedEdgeR = edgeFactory.makeJoinedEdge(edge, bestJoinedEdge);
	    		if (nextJoinedEdge.score < joinedEdgeR.score) {
	    			nextJoinedEdge = joinedEdgeR; nextFrag = edge;
	    		}
	    	}
	    	bestJoinedEdge = nextJoinedEdge;
	    	fragments.remove(nextFrag);
    	}
    }
    
    // returns edge as the new best frag if it doesn't intersect bitset 
    // and has a better completeness or better score with same completeness; 
    // otherwise returns bestFrag
    private Edge chooseBestFrag(BitSet bitset, Edge bestFrag, Edge edge) {
		if (edge.bitset.isEmpty() || edge.bitset.intersects(bitset)) return bestFrag;
        if (bestFrag == null) return edge;
        if (bestFrag.completeness < edge.completeness) return edge;
        if (bestFrag.completeness == edge.completeness && bestFrag.score < edge.score) return edge;
        return bestFrag;  
    }
    
    /** Unpack complete edges, if any; otherwise unpack all. */
	protected void doUnpacking() {
        Map<Identity<Edge>, Edge> unpacked = new LinkedHashMap<Identity<Edge>, Edge>();
	    boolean foundComplete = bestEdge.complete();
        // unpack each relevant edge, updating best edge 
        for (Edge edge : edges) {
            if (foundComplete && !edge.complete()) continue;
            unpack(edge, unpacked);
            updateBestEdge(edge.altEdges.get(0));
        }
    }
    
    // recursively unpack and prune edge, unless already visited
    private void unpack(Edge edge, Map<Identity<Edge>, Edge> unpacked) {
    	Identity<Edge> ei = new Identity<Edge>(edge);
    	if (unpacked.containsKey(ei)) {
        	return;
        }
        // add to unpacked set
        unpacked.put(ei, edge);
        // OR: recursively unpack alts, merging resulting alts
        Map<EdgeSurfaceWords, Edge> merged = new LinkedHashMap<EdgeSurfaceWords, Edge>();
	    if (edge.altEdges == null) {
	    	throw new RuntimeException("No alts for: " + edge);
	    }
        for (Edge alt : edge.altEdges) {
            // AND: unpack inputs, make alts, add to merged
            unpackAlt(alt, unpacked, merged);
        }
        // sort, rescore and prune
        List<Edge> mergedList = new ArrayList<Edge>(merged.values());
        Collections.sort(mergedList, edgeComparator);
        List<Edge> prunedEdges = pruningStrategy.pruneEdges(mergedList);
        prunedEdges.size();
        // replace edge's alts, add to unpruned edges
        edge.altEdges.clear(); edge.altEdges.addAll(mergedList);
        allEdges.addAll(mergedList);
        // update signMap (for debugging)
        for (Edge mergedEdge : mergedList) {
        	if (!signMap.containsKey(mergedEdge.sign))
        		signMap.put(mergedEdge.sign, mergedEdge);
        }
    }
    
    // recursively unpack inputs, make alt combos and add to merged
    private void unpackAlt(Edge alt, Map<Identity<Edge>, Edge> unpacked, Map<EdgeSurfaceWords, Edge> merged) {
        // first check for opt completed edge
        if (alt.optCompletes != null) {
            // recursively unpack input edge
            Edge inputEdge = alt.optCompletes;
            unpack(inputEdge, unpacked);
            // then make and merge alt edges from input alt edges
            for (Edge inputAlt : inputEdge.altEdges) {
                Edge edgeToAdd = (inputAlt.sign == alt.sign)
                    ? alt // use this alt for same sign
                    : edgeFactory.makeAltEdge(inputAlt.sign, alt); // otherwise make edge for new alt
                Chart.insertEdge(edgeToAdd, merged);
            }
            return;
        }
        // otherwise unpack via input signs
        DerivationHistory history = alt.sign.getDerivationHistory(); 
        Sign[] inputSigns = history.getInputs();
        // base case: no inputs
        if (inputSigns == null) {
            Chart.insertEdge(alt, merged); return;
        }
        // otherwise recursively unpack
        Edge[] inputEdges = new Edge[inputSigns.length];
        for (int i = 0; i < inputSigns.length; i++) {
            inputEdges[i] = signMap.get(inputSigns[i]); // get input edge using signMap
            unpack(inputEdges[i], unpacked);
        }
        // then make edges for new combos, and add to merged (if unseen)
        Category resultCat = alt.sign.getCategory();
        boolean lefthead = (alt.sign.getLexHead() == inputSigns[0].getLexHead());
        List<Sign[]> altCombos = inputCombos(inputEdges, 0);
        for (Sign[] combo : altCombos) {
        	Sign lexHead = (lefthead) ? combo[0].getLexHead() : combo[1].getLexHead();
            Sign sign = Sign.createDerivedSignWithNewLF(rdg, l, t, resultCat, combo, history.getRule(), lexHead);
            Edge edgeToAdd = (sign.equals(alt.sign))
                ? alt // use this alt for equiv sign
                : edgeFactory.makeAltEdge(sign, alt); // otherwise make edge for new alt
            Chart.insertEdge(edgeToAdd, merged);
        }
    }

    // returns a list of sign arrays, with each array of length inputEdges.length - i, 
    // representing all combinations of alt signs from i onwards
    private List<Sign[]> inputCombos(Edge[] inputEdges, int index) {
        Edge edge = inputEdges[index];
        // base case, inputEdges[last]
        if (index == inputEdges.length-1) {
            List<Edge> altEdges = edge.altEdges; 
            List<Sign[]> retval = new ArrayList<Sign[]>(altEdges.size());
            for (Edge alt : altEdges) {
                retval.add(new Sign[] { alt.sign });
            }
            return retval;
        }
        // otherwise recurse on index+1
        List<Sign[]> nextCombos = inputCombos(inputEdges, index+1);
        // and make new combos
        List<Edge> altEdges = edge.altEdges; 
        List<Sign[]> retval = new ArrayList<Sign[]>(altEdges.size() * nextCombos.size());
        for (Edge alt : altEdges) {
            for (int i = 0; i < nextCombos.size(); i++) {
                Sign[] nextSigns = nextCombos.get(i);
                Sign[] newCombo = new Sign[nextSigns.length+1];
                newCombo[0] = alt.sign;
                System.arraycopy(nextSigns, 0, newCombo, 1, nextSigns.length);
                retval.add(newCombo);
            }
        }
        return retval;
    }
    

    public List<Edge> bestEdges() {
        ArrayList<Edge> bestEdges = new ArrayList<Edge>();
        //no complete edges!
        if (!bestEdge.complete()) {
        	return bestEdges;
        }
        List<Edge> edgesToUse = rSet.doingUnpacking() ? edges : allEdges;
        for (Edge edge : edgesToUse) {
            if (edge.complete()) {
            	this.addSorted(bestEdges, edge); 
            }
        }
        pruningStrategy.pruneEdges(bestEdges);
        return bestEdges;
    }
    public Edge getBestEdge() {
    	return bestEdge;
    }
    
    public Realization getBestRealization(String goal) {
        String bracketedEdge = bestEdge.sign.getBracketedString();
        return new Realization(bracketedEdge.replace(")", "").replace("(", ""), goal, bestEdge.complete());
    }
    
    public String getBestEdgeDerivation() {
    	Edge edge = bestEdge;
    	//determines, basically, whether pruned edges are included
    	ArrayList<Edge> edgeList = rSet.doingUnpacking() ? new ArrayList<Edge>(edges) : new ArrayList<Edge>(allEdges); 
    	return edgeDerivation(edge, edgeList);
    	
    }
    private String edgeDerivation(Edge edge, List<Edge> edgeList) {
    	if (edgeList == null) {
    		return ""; //no way to determine the derivation!
    	}
    	DerivationHistory history = edge.sign.getDerivationHistory();
    	Sign[] inputs = history.getInputs();
    	if (inputs == null) { 
    		return " (lex)";
    	}
    	String retval = " (";  	
    	for (Sign sign : inputs) {
			Edge repEdge = signMap.get(sign); //if signmap has it, then it should be in edgelist
			if (repEdge != null) { 
				retval += edgeList.indexOf(repEdge) + " ";
			}
		}
		retval += history.getRule().name() + ")";
    	return retval;
    }

    // in the anytime case, first checks signs to see whether an edge whose sign is equivalent (up to surface words) and which has 
    // an equal or higher score or equal or lower derivational complexity has been seen already, and drops 
    // the given edge if so (in the packing case, this equivalence check is performed during unpacking);
    // if the edge replaces an (essentially) equivalent edge of lower score or higher 
    // derivational complexity, removes the old edge from the agenda or removes it from its equivalence class and puts it on a list of 
    // superceded edges to be pruned from the chart; then, in all cases, adds the given edge to the agenda, 
    // and updates the best edge so far, with preference given to completeness, then sign score
    private void addEdgeToAgenda(Edge edge) {
    	numEdges++;
    	if (!rSet.usingPacking()) {
	    	// update edgeHash, checking for equivalent edge of equal or lower complexity
    		
    		Edge ret = Chart.insertEdge(edge, this.edgeHash);
    		if (ret == null) {
    			return; //nothing was inserted, just leave.
    		}
    		Edge old = ret != edge ? ret : null; //let's find out if we need to remove it!
    		if (old != null) {
    			boolean onAgenda = agenda.remove(old);
    			if (!onAgenda) {
	    	        Edge repEdge = catMap.get(edge.getSemantics());//catMap.get(curEdge.getSemantics());
	    	        if (repEdge != null) {
	    	        	boolean inChart = repEdge.altEdges.remove(old);
	    	        	if (inChart) supercededEdgesPendingRemoval.add(old);
	    	        }
	    		}
    		}
    	}
        addSorted(agenda, edge);
        updateBestEdge(edge);
    }

    private void updateBestEdge(Edge edge) {
        //if there isn't a best edge, or if the edge's completeness is better, or if the completeness is the same but the score is higher
        if (bestEdge == null || bestEdge.completeness < edge.completeness || 
        (bestEdge.completeness == edge.completeness && edge.score > bestEdge.score)) {
            bestEdge = edge; 
        }
    }
    
    // removes superceded edges from the chart
    private void pruneSupercededEdges() {
    	for (Edge oldEdge : supercededEdgesPendingRemoval) {
    		allEdges.remove(oldEdge);
    	}
    	supercededEdgesPendingRemoval.clear();
    }
    
    // adds the edge to the chart and makes it a representative edge if it's the first one added for its equiv class; otherwise it's added as an alternative;
    // in the anytime case, prunes the edges listed as alts for the representative edge, nd adds the edge to the list of all unpruned edges, if it survives pruning;
    // returns true if the edge is actually added, and false if it doesn't survive the pruning (anytime case), or is folded into an existing edge (packing case);
    // prunes the edge and returns false if the cell count is exceeded
    private boolean addEdgeToChart(Edge edge) {
    	// check cell count
    	if (cellCount(edge) >= rSet.getCellPruningValue()) {
    		return false;
    	}
    	// inc cell count
    	incCellCount(edge);
        // get representative edge for this edge
        Edge repEdge = catMap.get(edge.getSemantics());//catMap.get(edge.getSemantics());
        // check for same edge already in chart; pretend it's been added
        if (edge == repEdge) { 
        	return true;
        }
        // if none, make this edge into one, adding it to the chart
        if (repEdge == null) {
            edge.initAltEdges(); // nb: could try capacity of pruningValue+1
            if (rSet.collectingCombos()) {
            	edge.initEdgeCombos();
            }
            //catMap.put(edge.getSemantics(), edge);
            catMap.put(edge.getSemantics(),  edge);
            edges.add(edge);
        	signMap.put(edge.sign, edge);
            // anytime case: add to all edges list too
            if (!rSet.usingPacking()) allEdges.add(edge);
            // and return
            return true;
        }
        // otherwise add edge to alts (sorted)
        else {
            addSorted(repEdge.altEdges, edge);
            // packing case: return false, as edge is simply folded into repEdge
            if (rSet.usingPacking()) return false;
        }
        
        //do pruning
        List<Edge> prunedEdges = pruningStrategy.pruneEdges(repEdge.altEdges);
        boolean edgeItselfPruned = false;
        for (Edge prunedEdge : prunedEdges) {
            if (prunedEdge != edge) {
                allEdges.remove(prunedEdge);
            }
            else edgeItselfPruned = true;
        }
        // add edge to all edges list, if it was not pruned
        if (!edgeItselfPruned) {
            allEdges.add(edge);
        	signMap.put(edge.sign, edge); // for debugging
            return true;
        }
        return false;
    }
    private static Edge insertEdge(Edge e, Map<EdgeSurfaceWords, Edge> map) {
    	EdgeSurfaceWords key = e.getSW();
    	Edge old = map.get(key);
    	if (old == null) {
    		map.put(key, e);
    		return e;
    	}
    	if (old == e) {
    		 return null;
    	}
    	if (e.score > old.score) {
    		map.put(key, e);
    		return old;
    	}
    	int complexity = e.sign.getDerivationHistory().complexity();
    	int oldComplexity = old.sign.getDerivationHistory().complexity();
        if (complexity < oldComplexity) {
             map.put(key, e);
             return old;
        }     
        return null;
    }
    
    // cell count
    private int cellCount(Edge edge) {
    	Integer count = cellMap.get(edge.bitset);
    	return (count == null) ? 0 : count;
    }

    // inc cell count
    private void incCellCount(Edge edge) {
    	int count = cellCount(edge);
    	cellMap.put(edge.bitset, ++count);
    	if (count > cellMax) {
    		cellMax = count;
    	}
    }
    
    private void addSorted(List<Edge> list, Edge edge) {
    	Comparator<Edge> comparator = (rSet.gluingFragments()) ? edgeSizeComparator : edgeComparator; 
        // do binary search
        int index = Collections.binarySearch(list, edge, comparator);
        // check if search found an edge with the same sort pos
        if (index >= 0) {
            // if so, advance the index past sort equiv edges
            while (index < list.size()) {
                Edge existingEdge = list.get(index);
                //if (existingEdge.score == edge.score) index++;
                if (comparator.compare(existingEdge, edge) == 0) index++;
                else break;
            }
        }
        else {
            // otherwise, convert index to insertion point
            index = Math.abs(index) - 1;
        }
        // then add edge at index
        list.add(index, edge); 
    }

    /** Compares edges based on their relative score, in descending order. */
    public static final Comparator<Edge> edgeComparator = new Comparator<Edge>() {
        public int compare(Edge edge1, Edge edge2) {
            return -1 * Double.compare(edge1.score, edge2.score);
        }
    };

    /** Compares edges based on their relative size then score, in descending order. */
    public static final Comparator<Edge> edgeSizeComparator = new Comparator<Edge>() {
        public int compare(Edge edge1, Edge edge2) {
        	int retval = -1 * Float.compare(edge1.completeness, edge2.completeness);
        	if (retval != 0) return retval;
            return -1 * Double.compare(edge1.score, edge2.score);
        }
    };
    
    /** The PrintWriter to use with the printing routines.  Default wraps System.out. */
    public PrintWriter log = new PrintWriter(System.out);
    
//    //prints sorted edges
//    public void printEdges(boolean complete) {
//        List<Edge> edgeList = rSet.doingUnpacking() ? edges : allEdges;
//        edgeList = new ArrayList<Edge>(edgeList);
//        //shouldn't need to sort! maintaining sort order always now
//        
//        for (int i=0; i < edgeList.size(); i++) {
//        	Edge edge = edgeList.get(i);
//            if (!complete || edge.complete()) {
//                printEdge(edge, null);
//            }
//
//        }
//        log.flush();
//    }
//    private void printEdge(Edge edge, List<Edge> edgeList) {
//        String str = edge.toString();
//        if (edge.incompleteLfChunk != null) {
//            int id = edgeFactory.lfChunks.indexOf(edge.incompleteLfChunk);
//            str += " <[" + id + "]>";
//        }
//        if (edge.activeLfAlts.size() > 0) str += " ";
//        for (List<Alt> altSet : edge.activeLfAlts) {
//            for (Alt alt : altSet) str += "?" + alt.altSet + "." + alt.numInSet;
//        }
//        str += edgeDerivation(edge, edgeList);
//        log.println(str);
//        // show alts subordinated in packing only case
//        if (rSet.doingUnpacking() && edge.isDisjunctive()) {
//            for (Edge alt : edge.altEdges) {
//                if (alt != edge)
//                        log.println(" \\_ " + alt + edgeDerivation(alt, edgeList));
//            }
//        }
//    }


}
    
