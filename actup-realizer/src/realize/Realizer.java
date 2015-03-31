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

import grammar.Grammar;
import hylo.HyloHelper;
import hylo.SatOp;

import java.util.List;

import org.jdom.Document;
import org.jdom.Element;

import synsem.LF;
import synsem.SignScorer;

/**
 * The realizer manages the realization process.
 * Realization options may be set for use across calls 
 * to the realizer.
 *
 * @author      Michael White
 * @version     $Revision: 1.31 $, $Date: 2011/07/19 03:40:46 $
 */
public class Realizer
{
 //   public static final long REALIZER_TIME_LIMIT = 0; //2 seconds, I believe
	public static final int REALIZER_ITER_LIMIT = 15000;
	
    /** The grammar used for realization. */
    public final Grammar grammar; 
    public boolean depthFirst;   
    // the chart used to realize a request
    private Chart chart;
    /** Sign scorer to use.  (Default is none.) */
    private SignScorer signScorer;
//    public long timeLimitMS;
    private int iterLimit;
    /** Flag for whether to wait for a complete edge. (Default is false.) */
    private boolean waitForCompleteEdge;
    /** Pruning strategy to use. (Default is none.) */
    private PruningStrategy pruningStrategy;
    /** Hypertagger to use. (Default is none.) */
   
    /** Constructor. */
    public Realizer(Grammar grammar) { 
        this.grammar = grammar;
     //   this.timeLimitMS = REALIZER_TIME_LIMIT;
        this.depthFirst = false;
        this.waitForCompleteEdge = false;
        this.pruningStrategy = new NBestPruningStrategy();;
        this.signScorer = null;
        this.iterLimit = REALIZER_ITER_LIMIT;
    }
    
    /** Returns the chart used in the latest request, or null if none. */
    public Chart getChart() { return chart; }
    
    //-----------------------------------------------------------------
    // get LF from doc    
    
    /**
     * Retrieves an input LF from the given XML doc, processing any 
     * LF chunks along the way.
     */
    public static LF getLfFromDoc(Document doc) {
        Element rootElt = doc.getRootElement();
        Element lfElt = (rootElt.getName().equals("lf")) ? rootElt : rootElt.getChild("lf");
        return getLfFromElt(lfElt);
    }

    /**
     * Retrieves an input LF from the given XML element, processing any 
     * LF chunks along the way.
     */
    public static LF getLfFromElt(Element lfElt) {
        HyloHelper.processChunks(lfElt);
        LF lf = HyloHelper.getLF(lfElt);
        return lf;
    }

    
    //-----------------------------------------------------------------
    // realization routines    
    
    /**
     * Realizes the input LF, 
     * returning the best edge found (or null if none).
     */
    public Edge realize(LF lf) {
        return realize(lf, this.signScorer);
    }

    /**
     * Realizes the input LF relative to the given sign scorer, 
     * returning the best edge found (or null if none).
     */
    public Edge realize(LF lf, SignScorer signScorer) {
    //	long timeLimitToUse = this.timeLimitMS;
        return realize(lf, signScorer, this.iterLimit, waitForCompleteEdge);
    }
    
    /**
     * Realizes the input LF relative to given sign scorer, 
     * returning the best edge found (or null if none)
     * in the given time limit (in ms), potentially waiting 
     * longer for a complete edge according to the given flag.
     * If a hypertagger is employed, realization proceeds 
     * iteratively through the available beta-best values 
     * within the overall time or edge limit.
     */
    public Edge realize(LF lf, SignScorer signScorer, int iterLimit, boolean waitForCompleteEdge) {
        List<SatOp> preds = HyloHelper.flatten(lf);
        SignScorer scorerToUse = signScorer;
        // realize iteratively with hypertagger, if present
        // otherwise make chart, set start time 
        chart = new Chart(new EdgeFactory(grammar, preds, scorerToUse), this.pruningStrategy);
        chart.depthFirst = depthFirst;
        // run request
        chart.initialize();
        chart.combine(iterLimit, waitForCompleteEdge);
        chart.printEdges(true, true);
        // return best edge
        return chart.bestEdge;
    }
}
