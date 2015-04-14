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

import org.jdom.Element;

import pruning.NBestPruningStrategy;
import pruning.PruningStrategy;
import runconfig.RealizationSettings;
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
    /** The grammar used for realization. */
    private final Grammar grammar;
    public Grammar getGrammar() {
    	return this.grammar;
    }
    private Chart chart;
    private PruningStrategy pruningStrategy;
    private final RealizationSettings rSet;
    
    /** Constructor. */
    public Realizer(RealizationSettings rs, Grammar grammar) { 
    	this.rSet = rs;
        this.grammar = grammar;
        this.pruningStrategy = new NBestPruningStrategy(rSet.getEdgePruningValue());
    }
    
    /** Returns the chart used in the latest request, or null if none. */
    public Chart getChart() { return chart; }

    /**
     * Retrieves an input LF from the given XML element, processing any 
     * LF chunks along the way.
     */
    public static LF getLfFromElt(Grammar grammar, Element lfElt) {
        HyloHelper.processChunks(lfElt);
        LF lf = HyloHelper.getLF(grammar, lfElt);
        return lf;
    }
    
    /**
     * Realizes the input LF relative to given sign scorer, returning the best edge found (or null if none)
     * in the given time limit (in ms), potentially waiting longer for a complete edge according to the given flag.
     * If a hypertagger is employed, realization proceeds iteratively through the available beta-best values 
     * within the overall time or edge limit.
     */
    public Edge realize(LF lf, SignScorer signScorer) {
    	boolean printOnlyComplete = true;
        List<SatOp> preds = HyloHelper.flatten(grammar, lf);
        SignScorer scorerToUse = signScorer;
        // realize iteratively with hypertagger, if present
        // otherwise make chart, set start time 
        chart = new Chart(rSet, new EdgeFactory(grammar, preds, scorerToUse), this.pruningStrategy);
        chart.initialize();
        chart.combine(rSet.getIterLimit());
        chart.printEdges(printOnlyComplete);
        return chart.getBestEdge();
    }
}
