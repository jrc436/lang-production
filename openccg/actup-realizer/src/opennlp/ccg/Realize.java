///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-7 University of Edinburgh, Michael White
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

package opennlp.ccg;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import opennlp.ccg.grammar.Grammar;
import opennlp.ccg.ngrams.ACTRNgramModel;
import opennlp.ccg.ngrams.StandardNgramModel;
import opennlp.ccg.realize.Chart;
import opennlp.ccg.realize.Realizer;
import opennlp.ccg.synsem.LF;
import opennlp.ccg.synsem.SignScorer;

import org.jdom.Document;
import org.jdom.Element;

/**
 * Sample front-end to the realizer, showing the intermediate steps of realization.
 *
 * @author      Michael White
 * @version     $Revision: 1.38 $, $Date: 2011/08/10 17:58:45 $
 */
public class Realize
{
    private PrintWriter out;


	public void realizeMain(boolean useACTR, String modelFile, String grammarfile, String inputfile, String outputfile) throws Exception {
        out = new PrintWriter(new BufferedWriter(new FileWriter(outputfile)));
        
        // load grammar
        URL grammarURL = new File(grammarfile).toURI().toURL();
        System.out.println("Loading grammar from URL: " + grammarURL);
        Grammar grammar = new Grammar(grammarURL);

        // instantiate realizer        
        Realizer realizer = new Realizer(grammar);
        
        // get request
        Document doc = grammar.loadFromXml(inputfile);
        out.flush();
        
        Element root = doc.getRootElement();
        @SuppressWarnings("unchecked")
		List<Element> items = root.getChildren("item");
        
        //remove unparsed nodes
        List<Element> toRemove = new ArrayList<Element>();
        for (Element item : items) {
        	String parseAttr = item.getAttribute("numOfParses").getValue();
        	if (parseAttr.equals("0")) {
        		toRemove.add(item);
        	}
        }
        for (Element remove : toRemove) {
        	items.remove(remove);
        }
        toRemove = null; 
        
        //build LM
//        List<String> targetList = Files.readAllLines(Paths.get(modelFile), Charset.defaultCharset());
//        String[] targets = new String[targetList.size()];
//        for (int i = 0; i < targets.length; i++) {
//        	targets[i] = targetList.get(i);
//        }
//        targetList = null;
//        SignScorer ngramScorer = new NgramPrecisionModel(targets);
        //SignScorer ngramScorer = ;
        SignScorer ngramScorer = useACTR ? new ACTRNgramModel(3, modelFile) : new StandardNgramModel(3, modelFile);
        
        //this uses the inputfile 
        String[] goals = new String[items.size()];
        for (int i = 0; i < items.size(); i++) {
        	goals[i] = items.get(i).getAttribute("string").getValue();
        }
        
        
        
        //realize strings
        for (int i = 0; i < items.size(); i++) {
        	Element item = items.get(i);
        	
        	Element lfelt = item.getChild("lf");
	        LF lf = Realizer.getLfFromElt(lfelt);
	        
	        out.println("Input LF: " + lf);
	        out.println("Goal: " + goals[i]);

	        realizer.realize(lf, ngramScorer);
	        
	        Chart chart = realizer.getChart();
	        chart.out = out;
	        
	        if (useACTR) {
	        	((ACTRNgramModel) ngramScorer).updateActivationTable(goals[i]);
	        }
	        
	        chart.printBestEdge();
	        out.println();
	    }
    }
}
