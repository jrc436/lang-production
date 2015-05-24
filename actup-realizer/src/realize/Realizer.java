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
import grammar.RuleGroupData;
import grammar.TypesData;
import hylo.HyloHelper;
import hylo.SatOp;

import java.util.List;

import lexicon.LexicalData;
import lexicon.Lexicon;

import org.jdom.Element;

import pruning.PruningStrategy;
import runconfig.RealizationSettings;
import synsem.LF;
import synsem.SignScorer;
import unify.UnifyControl;

/**
 * @author      Michael White
 * @version     $Revision: 1.31 $, $Date: 2011/07/19 03:40:46 $
 * 
 * Realizers are thread-specific handlers of the realization-process. RealizeMain is the thread-generic handler of the realizers.
 * As a thread-specific handler, here's a list of what it should and shouldn't keep track of:
 * 	An NgramScorer: This is more specific than the Thread-level. NgramScorers need be cleaned after every conversation, but they are also file-specific
 *                  Therefore, its' recommended to pass this in during the call to score, and let RealizeMain handle the scorers
 *   Immutable Run Data: The Lexicon, the Grammar, the Types, the Tokenizer, PruningStrategy, the WordFactory, the RuleGroup - all fine, since they're immutable, will receive from RealizeMain
 *   Mutable Run Data: UnifyControl, the LexicalData, the TypesData, the RuleGroupData - this is the same level. Each Realizer should have separate ones.
 */
public class Realizer
{	
    /** The grammar used for realization. */
    private final Grammar grammar; 
    private final PruningStrategy pruningStrategy;
    private final LexicalData lex;
    private final RuleGroupData rgd;
    private final TypesData td;
    private final UnifyControl uc;
    
    private final RealizationSettings rSet;

    protected Realizer(RealizationSettings rs, PruningStrategy ps, Grammar grammar) { 
    	this.rSet = rs;
        this.grammar = grammar;
        td = grammar.createNewTypesData();
        uc = grammar.createNewUnifyControl(td);       
        lex = grammar.createNewLexicalData(uc, td);
        rgd = grammar.createNewRuleGroupData(td, lex, uc);
        this.pruningStrategy = ps; 
    }
    
    /**
     * Retrieves an input LF from the given XML element, processing any 
     * LF chunks along the way.
     */
    public static LF getLfFromElt(Lexicon l, Element lfElt, TypesData td) {
        HyloHelper.processChunks(lfElt);
        LF lf = HyloHelper.getLF(l, td, lfElt);
        return lf;
    }
    
    /**
     * Realizes the input LF relative to given sign scorer, returning the best edge found (or null if none)
     * in the given time limit (in ms), potentially waiting longer for a complete edge according to the given flag.
     * If a hypertagger is employed, realization proceeds iteratively through the available beta-best values 
     * within the overall time or edge limit.
     */
    public Chart realize(LF lf, SignScorer signScorer) {
    //	boolean printOnlyComplete = true;
        List<SatOp> preds = HyloHelper.flatten(grammar.getLexicon(), td, lf);
        SignScorer scorerToUse = signScorer;
        // realize iteratively with hypertagger, if present
        // otherwise make chart, set start time 
        Chart chart = new Chart(rSet, new EdgeFactory(lex, uc, grammar.getLexicon(), rgd, grammar.getWordFactory(), td, grammar.getRuleGroup(), preds, scorerToUse, grammar.getTokenizer()), grammar.getLexicon(), rgd, this.pruningStrategy, grammar.getTokenizer());
        chart.initialize();
        chart.combine(rSet.getIterLimit());
      //  chart.printEdges(printOnlyComplete);
        return chart;
    }
}
