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

import gnu.trove.THashMap;
import gnu.trove.THashSet;
import gnu.trove.TObjectHashingStrategy;
import gnu.trove.TObjectIdentityHashingStrategy;
import hylo.Alt;
import hylo.SatOp;

import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import synsem.Category;
import synsem.DerivationHistory;
import synsem.Sign;

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
    public static final int NO_EDGE_LIMIT = 0;
    public static final int NO_PRUNING = 0; 
    
    //PREFERENCES
    public static final int EDGE_LIMIT = NO_EDGE_LIMIT;
   // public static final long TIME_LIMIT = NO_TIME_LIMIT;
    //public static final double NEW_BEST_TIME_LIMIT = NO_TIME_LIMIT;
    public static final int CELL_PRUNING_VALUE = NO_PRUNING;
    public static final int PRUNING_VALUE = NO_PRUNING;  
    public static final boolean DO_UNPACKING = true;    
    public static final boolean USE_COMBOS = true;   
    public static final boolean USE_PACKING = false;
    
    public long edgeLimit;    
    public int pruningValue;
    public int cellPruningValue;
    public boolean collectCombos;
    public boolean usePacking;
    public boolean doUnpacking;
 //   public long newBestTimeLimit;
 //   public double newBestTimeLimitPct;

    public boolean depthFirst = false;              
    /** Flag for whether to join best fragments if no complete realization found.  Defaults to false. */
    public boolean joinFragments = true;

    /** Flag for whether to glue fragments currently. Defaults to false. */
    public boolean gluingFragments = false;

    
    /** The edge factory for the realization request. */
    public final EdgeFactory edgeFactory;
    /** The pruning strategy. */
    public final PruningStrategy pruningStrategy;
    
    // the agenda of edges that have yet to be added to the chart
    private List<Edge> agenda = new ArrayList<Edge>();
    
    // the (representative) edges in the chart
    private List<Edge> edges = new ArrayList<Edge>();
    
    // all unpruned (and unpacked, if apropos) edges in the chart
    private List<Edge> allEdges = new ArrayList<Edge>();

    // edges to be removed from the chart, after having been superceded 
    // by an edge with an equivalent sign (up to surface words) and 
    // a less complex derivation
    private List<Edge> supercededEdgesPendingRemoval = new ArrayList<Edge>();

    // maps signs to edges (w/o optional bits marked as covered)
    private Map<Sign,Edge> signMap = new IdentityHashMap<Sign,Edge>();
    
    // the edges seen so far
    private EdgeHash edgeHash = new EdgeHash();
    

    // maps edges to representative edges, according to their 
    // coverage vectors and their cats, sans LFs
    @SuppressWarnings("unchecked")
    private Map<Edge, Edge> catMap = new THashMap(
        new TObjectHashingStrategy() {
			private static final long serialVersionUID = 1L;
			public int computeHashCode(Object o) {
                Edge edge = (Edge) o;
                return edge.bitset.hashCode() + edge.sign.getCategory().hashCodeNoLF();
            }
            public boolean equals(Object o1, Object o2) {
                Edge edge1 = (Edge) o1; Edge edge2 = (Edge) o2;
                return edge1.bitset.equals(edge2.bitset) &&
                    edge1.sign.getCategory().equalsNoLF(edge2.sign.getCategory());
            }
        }
    );
    
    // cell map: based on input coverage vectors
    private Map<BitSet,Integer> cellMap = new HashMap<BitSet,Integer>();
    
    // non-empty cells: cells to avoid when gluing fragments
    private Set<BitSet> nonEmptyCells  = null;
    
    // reusable bitset for checking non-empty cells
    private transient BitSet tmpBitSet = new BitSet();
    
    /** 
     * The best edge found so far (or null), 
     * where a complete edge is always given preference 
     * to an incomplete one.
     */
    public Edge bestEdge = null;
    
    /** The best edge created by joining fragments, if necessary. */
    public Edge bestJoinedEdge = null;
    
    /** Whether the realization search has been completed. */
    public boolean done = false;
    
    /** The number of nominals in the input LF. */
    public int numNominals = 0;
    
    /** The number of elementary predications in the input LF. */
    public int numPreds = 0;
    
    /** The number of edges created and added to the agenda. */
    public int numEdges = 0;
    
    /** The number of pruned edges removed from the chart. */
    public int numPrunedRemoved = 0;
    
    /** The number of pruned edges never added to the chart. */
    public int numPrunedNeverAdded = 0;
    
    /** The number of new complete best edges found after the first one. */
    public int newBest = 0;
    
    /** The maximum number of edges in a cell. */
    public int cellMax = 0;
    
//    /** The time at which realization started. */
//    protected long startTime = System.nanoTime();
//    /** The time in ms until lex lookup was completed. */
//    public long timeTilLex = 0;
//    /** The time in ms until the first complete edge was found. */
//    public long timeTilFirst = 0;
//    /** The time in ms until the best edge was found. */
//    public long timeTilBest = 0;
//    /** The time in ms until the search was stopped. */
//    public long timeTilStopped = 0;
//    /** The time in ms until the packed chart was completed. */
//    public long timeTilPacked = 0;
//    /** The time in ms until the search was finished. */
//    public long timeTilDone = 0;
//    
    
    /** 
     * Constructor with explicit pruning strategy. 
     */
     // * NB: Even with a non-default pruning strategy, it could potentially help 
     // *     to set the pruning value to an estimate of the number of 
     // *     edges per equivalent category that will be stored.
    public Chart(EdgeFactory edgeFactory, PruningStrategy pruningStrategy) {
        this.edgeFactory = edgeFactory;
        this.pruningStrategy = pruningStrategy;
//        newBestTimeLimitPct = TIME_LIMIT;
//        if (newBestTimeLimitPct >= 1) {
//            newBestTimeLimit = (int) newBestTimeLimitPct;
//            newBestTimeLimitPct = TIME_LIMIT; 
//        }
        edgeLimit = EDGE_LIMIT;
        pruningValue = PRUNING_VALUE;
        cellPruningValue = CELL_PRUNING_VALUE;
        usePacking = USE_PACKING; 
        collectCombos = !usePacking && USE_COMBOS;
        doUnpacking = usePacking && DO_UNPACKING;
    }
        
    
    /** Returns the number of (representative) edges in the chart. */
    public int numEdgesInChart() { return edges.size(); }

    /** Returns the number of unpruned (and unpacked, if apropos) edges in the chart. */
    public int numUnprunedEdges() { return allEdges.size(); }

    
    //-----------------------------------------------------------------
    // main algorithm routines    
    
    /** Initializes the agenda. */
    public void initialize() {
        // record number of nominals
        numNominals = edgeFactory.nominals.size();
        numPreds = edgeFactory.preds.size();
        // create various initial edges and add to the agenda
        for (Edge edge : edgeFactory.createInitialEdges())  
            addEdgeToAgenda(edge);
        // record time 'til lex
//        long currentTime = System.nanoTime();
//        timeTilLex = currentTime - startTime;
    }
    
    /** Returns whether there were no uncovered lexical or featural preds after lex lookup. */
    public boolean noUncoveredPreds() { return !edgeFactory.hasUncoveredPreds; }
    
    
    /** 
     * Reinitializes the agenda for gluing fragments.  
     * A runtime exception is thrown if not in packing mode.
     */
    public void reInitForGluing() {
    	// check packing mode
    	if (!usePacking) throw new RuntimeException("Packing mode required for gluing fragments.");
    	// set flags here and in edge factory
    	gluingFragments = true; 
    	edgeFactory.gluingFragments = true; edgeFactory.useIndexing = false;
    	// add opt for uncovered preds, unless already done for relaxed relation matching
    	if (!edgeFactory.useRelaxedRelationMatching)
    		edgeFactory.addLFOptsForUncoveredPreds();
    	// add opts for rule instances
    	edgeFactory.addLFOptsForRuleInstances();
    	// record non-empty cells
    	nonEmptyCells = new HashSet<BitSet>(cellMap.keySet());
    	// add edges back to agenda, for possible gluing
    	for (Edge edge : edges) addEdgeToAgenda(edge);
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
    public void combine(int iterLim, boolean waitForCompleteEdge) {
        
        // until agenda is empty
    	int currentIter = 0;
        while (!agenda.isEmpty()) {

            // check for timeout
//            long currentTime = System.nanoTime();
//            long timeSoFar = currentTime - startTime;
//            long timeSinceFirst = timeSoFar - timeTilFirst;
           
            boolean bestEdgeComplete = (bestEdge != null && bestEdge.complete()); 
            if (
//            	// edge limit case
            	(edgeLimit != NO_EDGE_LIMIT && numEdges > edgeLimit && (!waitForCompleteEdge || bestEdgeComplete))
            	||
//                // timeout case
//                (timeLimitMS != NO_TIME_LIMIT && timeSoFar > timeLimitMS && (!waitForCompleteEdge || bestEdgeComplete))
//                ||
                (currentIter > iterLim && (!waitForCompleteEdge || bestEdgeComplete))
//                ||
//                // new best timeout case (anytime only)
//                (!usePacking && bestEdgeComplete && ((newBestTimeLimit != NO_TIME_LIMIT && timeSinceFirst > newBestTimeLimit) ||
//                  (newBestTimeLimitPct != NO_TIME_LIMIT && 
//                   (double) timeSinceFirst / (double) timeTilFirst > newBestTimeLimitPct)))
            ) {
                // ensure best edge in chart
                if (!allEdges.contains(bestEdge)) { addEdgeToChart(bestEdge); }
                // set timing
//                timeTilStopped = timeSoFar;
                // stop
                break;
            }
            
            // take edge from agenda
            Edge next = agenda.remove(0);
            
            // add edge to chart
            boolean actuallyAdded = addEdgeToChart(next);
            
            // skip if edge didn't survive pruning (anytime case), 
            // or was folded into an existing edge (packing case)
            if (!actuallyAdded) { continue; }
            
            // otherwise do combos 
            doEdgeCombos(next);
            currentIter++;
        }

        // set done packing time
        if (usePacking) {
        //    long donePackingTime = System.nanoTime();
        //    timeTilPacked = donePackingTime - startTime;
            // do unpacking, if apropos
            if (doUnpacking) doUnpacking();
        }
        
        // set done flag, timing
        done = agenda.isEmpty();
        if (done) {
	   //     long endTime = System.nanoTime();
	   //     timeTilDone = endTime - startTime;
        }
        
        // join best fragments, if nec.
        if (joinFragments && !bestEdge.complete()) joinBestFragments(); 
    }
    
    // does binary combinations with all edges in the chart and unary ones too; 
    // when collecting combos, invokes the combinatory rules only when nec.; 
    // prunes superceded edges before returning
    private void doEdgeCombos(Edge next) {
    	// skip semantically null edges when gluing fragments
    	if (gluingFragments && next.bitset.isEmpty()) return;
    	// when collecting combos ...
        if (collectCombos) {
            // existing rep case: just make alt edges from collected combos
            Edge nextRep = catMap.get(next);
            if (next != nextRep) {
                addNewEdges(edgeFactory.createAltEdges(next, nextRep));
                // and prune any superceded edges before returning
                pruneSupercededEdges();
                return;
            }
        }
        // otherwise combine edge with those in chart 
        List<Edge> edgesToUse = (usePacking || collectCombos) ? edges : allEdges;
        for (Edge edge : edgesToUse) {
            if (edge == next) continue; // skip this edge
            // skip fragment gluing if semantically null or if result cell non-empty
            if (gluingFragments) {
            	if (edge.bitset.isEmpty()) continue;
            	tmpBitSet.clear();
            	tmpBitSet.or(edge.bitset); tmpBitSet.or(next.bitset);
            	if (nonEmptyCells.contains(tmpBitSet)) continue;
            }
            // add new combos to agenda
            addNewEdges(edgeFactory.createNewEdges(edge, next, collectCombos));
        }
        // combine edge via unary rules and with semantically null edges, 
        // adding new edges to the agenda
    	addNewEdges(edgeFactory.createNewEdges(next, collectCombos));
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
    
    
    
    //-----------------------------------------------------------------
    // unpacking    
    
    /** Unpack complete edges, if any; otherwise unpack all. */
	protected void doUnpacking() {
	    @SuppressWarnings("unchecked")
        Set<Edge> unpacked = new THashSet(new TObjectIdentityHashingStrategy());
	    boolean foundComplete = bestEdge.complete();
        // unpack each relevant edge, updating best edge 
        for (Edge edge : edges) {
            if (foundComplete && !edge.complete()) continue;
            unpack(edge, unpacked);
            updateBestEdge(edge.altEdges.get(0));
        }
    }
    
    // recursively unpack and prune edge, unless already visited
    private void unpack(Edge edge, Set<Edge> unpacked) {
        if (unpacked.contains(edge)) return;
        // add to unpacked set
        unpacked.add(edge);
        // OR: recursively unpack alts, merging resulting alts
        EdgeHash merged = new EdgeHash();
	    if (edge.altEdges == null) {
	    	throw new RuntimeException("No alts for: " + edge);
	    }
        for (Edge alt : edge.altEdges) {
            // AND: unpack inputs, make alts, add to merged
            unpackAlt(alt, unpacked, merged);
        }
        // sort, rescore and prune
        List<Edge> mergedList = new ArrayList<Edge>(merged.asEdgeSet());
        Collections.sort(mergedList, edgeComparator);
        List<Edge> prunedEdges = pruningStrategy.pruneEdges(mergedList);
        numPrunedNeverAdded += prunedEdges.size();
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
    private void unpackAlt(Edge alt, Set<Edge> unpacked, EdgeHash merged) {
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
                merged.insert(edgeToAdd);
            }
            return;
        }
        // otherwise unpack via input signs
        DerivationHistory history = alt.sign.getDerivationHistory(); 
        Sign[] inputSigns = history.getInputs();
        // base case: no inputs
        if (inputSigns == null) {
            merged.insert(alt); return;
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
            Sign sign = Sign.createDerivedSignWithNewLF(resultCat, combo, history.getRule(), lexHead);
            Edge edgeToAdd = (sign.equals(alt.sign))
                ? alt // use this alt for equiv sign
                : edgeFactory.makeAltEdge(sign, alt); // otherwise make edge for new alt
            merged.insert(edgeToAdd);
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
    
    
    //-----------------------------------------------------------------
    // best edges (single best is available directly as bestEdge)
    
    // cached best edges
    private transient List<Edge> bestEdges = null;
    
    /** 
     * Returns the best complete edges, sorted by their score and 
     * pruned by the pruning strategy. 
     */
    public List<Edge> bestEdges() {
    	if (bestEdges != null) return bestEdges;
        bestEdges = new ArrayList<Edge>();
        if (!bestEdge.complete()) return bestEdges;
        List<Edge> edgesToUse = (usePacking && !doUnpacking) ? edges : allEdges;
        for (Edge edge : edgesToUse) {
            if (edge.complete()) bestEdges.add(edge); 
        }
        Collections.sort(bestEdges, edgeComparator);
        pruningStrategy.pruneEdges(bestEdges);
        return bestEdges;
    }
    
    
    //-----------------------------------------------------------------
    // printing routines
    
    /** The PrintWriter to use with the printing routines.  Default wraps System.out. */
    public PrintWriter out = new PrintWriter(System.out);

    /** Prints the best edge found. */
    public void printBestEdge() {
        printEdge(bestEdge);
        if (!edgeFactory.labeledNominals.isEmpty()) {
            try {
                ByteArrayOutputStream bstr = new ByteArrayOutputStream();
                edgeFactory.grammar.serializeXml(
                    bestEdge.sign.getWordsInXml(edgeFactory.labeledNominals), bstr
                );
                out.println(bstr.toString());
            }
            catch (java.io.IOException exc) { 
                throw (RuntimeException) new RuntimeException().initCause(exc);
            }
        }
        out.println("Best Edge:" + bestEdge.sign.getBracketedString());
        out.flush();
    }
    
    public Realization getBestRealization(String goal) {
//    	printEdge(bestEdge);
//        if (!edgeFactory.labeledNominals.isEmpty()) {
//            try {
//                ByteArrayOutputStream bstr = new ByteArrayOutputStream();
//                edgeFactory.grammar.serializeXml(
//                    bestEdge.sign.getWordsInXml(edgeFactory.labeledNominals), bstr
//                );
//                out.println(bstr.toString());
//            }
//            catch (java.io.IOException exc) { 
//                throw (RuntimeException) new RuntimeException().initCause(exc);
//            }
//        }
        String bracketedEdge = bestEdge.sign.getBracketedString();
        return new Realization(bracketedEdge.replace(")", "").replace("(", ""), goal, bestEdge.complete());
    }
    
    /** Prints the best joined edge. */
    public void printBestJoinedEdge() {
    	if (bestJoinedEdge == null) return;
        printEdge(bestJoinedEdge);
        out.println(bestJoinedEdge.sign.getBracketedString());
        out.flush();
    }
    
//    /** Prints the timing (and related) info. */
//    public void printTiming() {
//        out.println();
//        if (!usePacking) {
//            if (bestEdge != null && bestEdge.complete())
//                out.println("time 'til first   (ms): " + timeTilFirst);
//            if (bestEdge != null)
//                out.println("time 'til best    (ms): " + timeTilBest);
//            if (timeTilStopped != 0)
//                out.println("time 'til stopped (ms): " + timeTilStopped);
//        }
//        else {
//            out.println("time 'til packed  (ms): " + timeTilPacked); 
//        }
//        if (timeTilDone != 0)
//            out.println("time 'til done    (ms): " + timeTilDone); 
//        out.println();
//        out.println("rule apps:   " + edgeFactory.ruleApps());
//        out.println("# edges:     " + edges.size());
//        out.println("# unpruned edges:     " + allEdges.size());
//        if (!usePacking) {
//            out.println("# pruned:    " + numPrunedRemoved + " removed, " + numPrunedNeverAdded + " never added");
//        }
//        if (doUnpacking) {
//            out.println("# pruned:    " + numPrunedNeverAdded);
//        }
//        out.println("cell max:    " + cellMax);
//        out.flush();
//    }
    
    /** Prints all chart edges, unsorted. */
    public void printEdges() { printEdges(false); }
    
    /** Prints chart edges unsorted, using the complete edges filter according to the given flag. */ 
    public void printEdges(boolean complete) { printEdges(complete, false); }
    
    /** 
     * Prints chart edges using the complete edges filter according to the given flag 
     * and sorting according to the given flag.
     * In the packing only case, the representative edges are shown, otherwise 
     * the unpruned (and possibly unpacked) edges are shown.
     */ 
    public void printEdges(boolean complete, boolean sort) {
        List<Edge> edgeList = (usePacking && !doUnpacking) ? edges : allEdges;
        if (sort) {
            edgeList = new ArrayList<Edge>(edgeList);
            Collections.sort(edgeList, edgeComparator);
        }
        for (int i=0; i < edgeList.size(); i++) { 
        	Edge edge = edgeList.get(i);
            if (!complete || edge.complete()) {
            	if (!sort) printEdge(edge, i, edgeList);
            	else printEdge(edge);
            }
                
        }
        out.flush();
    }

    /**
     * Prints the agenda.
     */
    public void printAgenda() {
        for (Edge edge : agenda) {
            printEdge(edge);
        }
        out.flush();
    }
    
    /**
     * Prints the initial edges.
     */
    public void printInitialEdges() {
        for (Edge edge : edgeFactory.initialEdges) {
            printEdge(edge);
        }
        out.flush();
    }
    
    // prints edge with incomplete LF chunks and active alts
    private void printEdge(Edge edge) { printEdge(edge, -1, null); }
    
    // prints also with edge index and derivation, if index non-negative
    private void printEdge(Edge edge, int index, List<Edge> edgeList) {
        String str = "";
        if (index >= 0) str += index + ". ";
        str += edge.toString();
        if (edge.incompleteLfChunk != null) {
            int id = edgeFactory.lfChunks.indexOf(edge.incompleteLfChunk);
            str += " <[" + id + "]>";
        }
        if (edge.activeLfAlts.size() > 0) str += " ";
        for (List<Alt> altSet : edge.activeLfAlts) {
            for (Alt alt : altSet) str += "?" + alt.altSet + "." + alt.numInSet;
        }
        str += edgeDerivation(edge, index, edgeList);
        out.println(str);
        // show alts subordinated in packing only case
        if (usePacking && !doUnpacking && edge.isDisjunctive()) {
            for (Edge alt : edge.altEdges) {
                if (alt != edge) 
                	out.println(" \\_ " + alt + edgeDerivation(alt, index, edgeList));
            }
        }
    }
    
    // returns derivation, if index non-negative
    private String edgeDerivation(Edge edge, int index, List<Edge> edgeList) {
    	if (index < 0) return "";
    	if (edge.optCompletes != null) {
    		return " (" + edgeList.indexOf(edge.optCompletes) + " optC)";
    	}
    	DerivationHistory history = edge.sign.getDerivationHistory();
    	Sign[] inputs = history.getInputs();
    	if (inputs == null) return " (lex)";
    	String retval = " (";
		for (Sign sign : inputs) {
			Edge repEdge = signMap.get(sign);
			if (repEdge != null) retval += edgeList.indexOf(repEdge) + " ";
		}
		retval += history.getRule().name() + ")";
    	return retval;
    }

    
    /**
     * Prints the licensed, marked initial edges.
     */
    public void printMarkedEdges() {
        for (Edge edge : edgeFactory.markedEdges) {
            printEdge(edge);
        }
        out.flush();
    }
    
    /**
     * Prints the licensed, instantiated purely syntactic (semantically null) edges.
     */
    public void printInstantiatedNoSemEdges() {
        for (Edge edge : edgeFactory.instantiatedNoSemEdges) {
            printEdge(edge);
        }
        out.flush();
    }
    
    /**
     * Prints the licensed, uninstantiated purely syntactic (semantically null) edges.
     */
    public void printNoSemEdges() {
        for (Edge edge : edgeFactory.noSemEdges) {
            out.println(edge.toString());
        }
        out.flush();
    }
    
    /**
     * Prints the rule instances, with instantiated semantics.
     */
    public void printRuleInstances() {
        for (Iterator<?> it = edgeFactory.ruleInstances.iterator(); it.hasNext(); ) {
            out.println(it.next().toString());
        }
        out.flush();
    }
    
    /**
     * Prints the LF chunks.
     */
    public void printLfChunks() {
        List<BitSet> chunks = edgeFactory.lfChunks;
        for (int i = 0; i < chunks.size(); i++) {
            BitSet chunk = chunks.get(i);
            out.println("chunk[" + i + "]:  " + Edge.toString(chunk));
        }
        out.flush();
    }
    
    /**
     * Prints the LF alternatives.
     */
    public void printLfAlts() {
        for (List<Alt> altSet : edgeFactory.lfAlts) {
            for (Alt alt : altSet) {
                out.print("alt[" + alt.altSet + "." + alt.numInSet + "]: ");
                out.println(Edge.toString(alt.bitset));
            }
        }
        out.flush();
    }
    
    /**
     * Prints the LF optional parts.
     */
    public void printLfOpts() {
        List<BitSet> opts = edgeFactory.lfOpts;
        for (int i = 0; i < opts.size(); i++) {
            BitSet opt = opts.get(i);
            out.println("opt[" + i + "]:  " + Edge.toString(opt));
        }
        out.flush();
    }
    
    /**
     * Prints the elementary predications.
     */
    public void printEPs() {
        List<SatOp> preds = edgeFactory.preds; 
        for (int i=0; i < preds.size(); i++) {
            SatOp lf_i = preds.get(i);
            out.println("ep[" + i + "]:  " + lf_i);
        }
        out.flush();
    }
    
    
    
    //-----------------------------------------------------------------
    // chart management

    // in the anytime case, first checks signs to see whether an edge 
    // whose sign is equivalent (up to surface words) and which has 
    // an equal or higher score or equal 
    // or lower derivational complexity has been seen already, and drops 
    // the given edge if so (in the packing case, this equivalence check is 
    // performed during unpacking);
    // if the edge replaces an (essentially) equivalent edge of lower score or higher 
    // derivational complexity, removes the old edge from the agenda 
    // or removes it from its equivalence class and puts it on a list of 
    // superceded edges to be pruned from the chart;
    // then, in all cases, adds the given edge to the agenda, 
    // and updates the best edge so far, with preference given to completeness, 
    // then sign score
    private void addEdgeToAgenda(Edge edge) {
    	numEdges++;
    	if (!usePacking) {
	    	// update edgeHash, checking for equivalent edge of equal or lower complexity
    		Edge retEdge = edgeHash.insert(edge);
    		boolean actuallyInserted = (retEdge != null);
	    	if (!actuallyInserted) { return; } // just drop it
	    	// remove old edge, if apropos
	    	Edge oldEdge = (retEdge != edge) ? retEdge : null; 
	    	if (oldEdge != null) {
	    		// check agenda first
	    		boolean onAgenda = agenda.remove(oldEdge);
	    		// if not on agenda, remove from equiv class, if present, 
	    		// and add to list of superceded edges pending removal
	    		// nb: delaying pruning of superceded edges from chart 
	    		//     is nec. to avoid a problem with concurrent access 
	    		//     to allEdges in doEdgeCombos
	    		if (!onAgenda) {
	    	        Edge repEdge = catMap.get(oldEdge);
	    	        if (repEdge != null) {
	    	        	boolean inChart = repEdge.altEdges.remove(oldEdge);
	    	        	if (inChart) supercededEdgesPendingRemoval.add(oldEdge);
	    	        }
	    		}
	    	}
    	}
        if (depthFirst) { agenda.add(0, edge); }
        else if (edge.score == 0) { agenda.add(edge); }
        else { addSorted(agenda, edge); }
        updateBestEdge(edge);
    }
    
    // update bestEdge wrt given edge, and adjust timing info
    private void updateBestEdge(Edge edge) {
        if (bestEdge == null) {
            bestEdge = edge; 
//            long endTime = System.nanoTime();
//            timeTilBest = endTime - startTime;
//            if (bestEdge.complete()) timeTilFirst = timeTilBest; 
            return;
        }
        if (bestEdge.completeness > edge.completeness) return;
        if (bestEdge.completeness < edge.completeness) {
            bestEdge = edge; 
//            long endTime = System.nanoTime();
//            timeTilBest = endTime - startTime;
//            if (bestEdge.complete()) timeTilFirst = timeTilBest; 
            return;
        }
        if (edge.score > bestEdge.score) {
            bestEdge = edge;
//            long endTime = System.nanoTime();
//            timeTilBest = endTime - startTime;
            if (bestEdge.complete()) newBest++;
        }
    }
    
    // removes superceded edges from the chart
    private void pruneSupercededEdges() {
    	for (Edge oldEdge : supercededEdgesPendingRemoval) {
    		allEdges.remove(oldEdge); numPrunedRemoved++;
    	}
    	supercededEdgesPendingRemoval.clear();
    }
    
    // adds the edge to the chart and makes it a representative edge if it's the 
    // first one added for its equiv class; otherwise it's added as an alternative;
    // in the anytime case, prunes the edges listed as alts for the representative edge, 
    // and adds the edge to the list of all unpruned edges, if it survives pruning;
    // returns true if the edge is actually added, and false if it doesn't survive 
    // the pruning (anytime case), or is folded into an existing edge (packing case);
    // prunes the edge and returns false if the cell count is exceeded
    private boolean addEdgeToChart(Edge edge) {
    	// check cell count
    	if (cellPruningValue != NO_PRUNING && cellCount(edge) >= cellPruningValue) {
    		numPrunedNeverAdded++; return false;
    	}
    	// inc cell count
    	incCellCount(edge);
        // get representative edge for this edge
        Edge repEdge = catMap.get(edge);
        // check for same edge already in chart; pretend it's been added
        if (edge == repEdge) return true;
        // if none, make this edge into one, adding it to the chart
        if (repEdge == null) {
            edge.initAltEdges(); // nb: could try capacity of pruningValue+1
            if (collectCombos) edge.initEdgeCombos();
            catMap.put(edge, edge);
            edges.add(edge);
        	signMap.put(edge.sign, edge);
            // anytime case: add to all edges list too
            if (!usePacking) allEdges.add(edge);
            // and return
            return true;
        }
        // otherwise add edge to alts (sorted)
        else {
            addSorted(repEdge.altEdges, edge);
            // packing case: return false, as edge is simply folded into repEdge
            if (usePacking) return false;
        }
        // anytime case: if not pruning, just add edge to all edges list, and return
        if (pruningValue == NO_PRUNING) {
            allEdges.add(edge);
        	signMap.put(edge.sign, edge); // for debugging
            return true;
        }
        // otherwise do pruning
        List<Edge> prunedEdges = pruningStrategy.pruneEdges(repEdge.altEdges);
        boolean edgeItselfPruned = false;
        for (Edge prunedEdge : prunedEdges) {
            if (prunedEdge != edge) {
                allEdges.remove(prunedEdge);
                numPrunedRemoved++;
            }
            else edgeItselfPruned = true;
        }
        // add edge to all edges list, if it was not pruned
        if (!edgeItselfPruned) {
            allEdges.add(edge);
        	signMap.put(edge.sign, edge); // for debugging
            return true;
        }
        // otherwise false
        numPrunedNeverAdded++;
        return false;
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
    	if (count > cellMax) cellMax = count;
    }
    
    
    //-----------------------------------------------------------------
    // edge sorted insertion and comparison
    
    // adds the given edge into the already sorted list, 
    // maintaining the sort order;
    // when gluing fragments, edges are sorted first by size, 
    // otherwise by score
    private void addSorted(List<Edge> list, Edge edge) {
    	Comparator<Edge> comparator = (gluingFragments) ? edgeSizeComparator : edgeComparator; 
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
}
    
