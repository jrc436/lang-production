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
import java.util.List;
import java.util.prefs.Preferences;

import opennlp.ccg.grammar.Grammar;
import opennlp.ccg.ngrams.NgramPrecisionModel;
import opennlp.ccg.realize.Chart;
import opennlp.ccg.realize.Edge;
import opennlp.ccg.realize.Hypertagger;
import opennlp.ccg.realize.Realizer;
import opennlp.ccg.realize.hypertagger.ZLMaxentHypertagger;
import opennlp.ccg.synsem.LF;
import opennlp.ccg.synsem.SignScorer;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.Format;

/**
 * Sample front-end to the realizer, showing the intermediate steps of realization.
 *
 * @author      Michael White
 * @version     $Revision: 1.38 $, $Date: 2011/08/10 17:58:45 $
 */
public class Realize
{
	
	
    private PrintWriter out;
    
    public void realizeMain(String grammarfile, String inputfile, String outputfile) throws Exception {
    	realizeMain(grammarfile, inputfile, outputfile, false);
    }
    
    public void realizeMain(String grammarfile, String inputfile, String outputfile, boolean exactMatches) throws Exception {
    	realizeMain(grammarfile, inputfile, outputfile, exactMatches, 0);
    }   

	public void realizeMain(String grammarfile, String inputfile, String outputfile, boolean exactMatches, int ngramOrder) throws Exception {
        out = new PrintWriter(new BufferedWriter(new FileWriter(outputfile)));
        
        // remember, modify prefs
        Preferences prefs = Preferences.userNodeForPackage(TextCCG.class);
        boolean oldShowCompleteness = prefs.getBoolean(Edge.SHOW_COMPLETENESS, false);
        boolean oldShowBitset = prefs.getBoolean(Edge.SHOW_BITSET, false);
        prefs.putBoolean(Edge.SHOW_COMPLETENESS, true);
        prefs.putBoolean(Edge.SHOW_BITSET, true);
        
        // load grammar
        URL grammarURL = new File(grammarfile).toURI().toURL();
        System.out.println("Loading grammar from URL: " + grammarURL);
        Grammar grammar = new Grammar(grammarURL);

        // instantiate realizer        
        Realizer realizer = new Realizer(grammar);
        
        // get request
        //out.println();
        //out.println("Request:");
        //out.println();
        Document doc = grammar.loadFromXml(inputfile);
        org.jdom.output.XMLOutputter outputter = new org.jdom.output.XMLOutputter(Format.getPrettyFormat()); 
        out.flush();
      //  outputter.output(doc, out);
      //  out.flush();
        
        //this is for a single run. Setting it up for multiple runs.
        
        
        Element root = doc.getRootElement();
        @SuppressWarnings("unchecked")
		List<Element> items = root.getChildren("item");
        
        
        for (Element item : items) {
        	String parseAttr = item.getAttribute("numOfParses").getValue();
        	if (parseAttr.equals("0")) {
        		continue;
        	}
        	//System.out.println(parseAttr);
        	Element lfelt = item.getChild("lf");
	        LF lf = Realizer.getLfFromElt(lfelt);
	        //out.println();
	        //out.println("** Initial run");
	        //out.println();
	        out.println("Input LF: " + lf);
	        
	        // set up n-gram scorer
	        SignScorer ngramScorer;
	        //Element root = doc.getRootElement();
	        //Element ngramModelElt = root.getChild("ngram-model");
	            // just use targets
	        String[] targets = new String[] { item.getAttribute("string").getValue() }; //not sure how it could be more than one
	        //out.println();
	        out.println("Target: " + targets[0]);
	       /* for (int i=0; i < targets.length; i++) {;
	            out.println(targets[i]);
	        }*/
	        ngramScorer = new NgramPrecisionModel(targets);
	        
	        // set hypertagger (if any)
	        Element htModelElt = root.getChild("ht-model");
	        if (htModelElt != null) {
	            String htconfig = htModelElt.getAttributeValue("config");
	            if (htconfig != null) {
	                out.println();
	                out.println("Instantiating hypertagger from: " + htconfig);
	            	realizer.hypertagger = ZLMaxentHypertagger.ZLMaxentHypertaggerFactory(htconfig);
	            }
	            else {
		            String htModelClass = htModelElt.getAttributeValue("class");
		            out.println();
		            out.println("Instantiating hypertagger from class: " + htModelClass);
		            realizer.hypertagger = (Hypertagger) Class.forName(htModelClass).newInstance();
	            }
	        }
	
	        realizer.realize(lf, ngramScorer);
	        Chart chart = realizer.getChart();
	        chart.out = out;

	        chart.printBestEdge();
	        out.println();
	        
	        // reset prefs
	        prefs.putBoolean(Edge.SHOW_COMPLETENESS, oldShowCompleteness);
	        prefs.putBoolean(Edge.SHOW_BITSET, oldShowBitset);
	    }
    }
}
