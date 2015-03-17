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

package opennlp.ccg.realize;

import java.util.List;

import opennlp.ccg.grammar.Grammar;
import opennlp.ccg.hylo.HyloHelper;
import opennlp.ccg.hylo.SatOp;
import opennlp.ccg.synsem.LF;
import opennlp.ccg.synsem.SignScorer;

import org.jdom.Document;
import org.jdom.Element;

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
    public static final int REALIZER_TIME_LIMIT = 15000;
	
	
    /** The grammar used for realization. */
    public final Grammar grammar; 
    public boolean depthFirst;   
    // the chart used to realize a request
    private Chart chart;
    /** Sign scorer to use.  (Default is none.) */
    public SignScorer signScorer;
    public int timeLimitMS;
    /** Flag for whether to wait for a complete edge. (Default is false.) */
    public boolean waitForCompleteEdge;
    /** Pruning strategy to use. (Default is none.) */
    public PruningStrategy pruningStrategy;
    /** Hypertagger to use. (Default is none.) */
    public Hypertagger hypertagger;
   
    /** Constructor. */
    public Realizer(Grammar grammar) { 
        this.grammar = grammar;
        this.timeLimitMS = REALIZER_TIME_LIMIT;
        this.depthFirst = false;
        this.waitForCompleteEdge = false;
        this.pruningStrategy = null;
        this.signScorer = null;
        this.hypertagger = null;
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
    	int timeLimitToUse = this.timeLimitMS;
        return realize(lf, signScorer, timeLimitToUse, waitForCompleteEdge);
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
    public Edge realize(LF lf, SignScorer signScorer, int timeLimitMS, boolean waitForCompleteEdge) {
        List<SatOp> preds = HyloHelper.flatten(lf);
        SignScorer scorerToUse = signScorer;
        PruningStrategy strategyToUse = new NBestPruningStrategy();
        // realize iteratively with hypertagger, if present
        // otherwise make chart, set start time
        long startTime = System.currentTimeMillis(); 
        chart = new Chart(new EdgeFactory(grammar, preds, scorerToUse), strategyToUse);
        chart.startTime = startTime; 
        chart.depthFirst = depthFirst;
        // run request
        chart.initialize();
        chart.combine(timeLimitMS, waitForCompleteEdge);
        //int numEdges = chart.numEdgesInChart();
        chart.printEdges(true, true);
        // XXX tmp
    	// if no complete edge, try again gluing fragments
//        if (!chart.bestEdge.complete()) {
//        	System.out.println("Trying to glue fragments ...");
//        	chart.reInitForGluing();
//        	chart.combine(timeLimitMS, waitForCompleteEdge);
//        }
        // return best edge
        return chart.bestEdge;
    }
}
