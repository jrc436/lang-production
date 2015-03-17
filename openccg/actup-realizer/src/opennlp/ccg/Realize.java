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
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import opennlp.ccg.grammar.Grammar;
import opennlp.ccg.ngrams.ACTRNgramModel;
import opennlp.ccg.ngrams.AbstractStandardNgramModel;
import opennlp.ccg.ngrams.StandardNgramModel;
import opennlp.ccg.realize.Chart;
import opennlp.ccg.realize.Realizer;
import opennlp.ccg.synsem.LF;

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
    private Grammar grammar;
    private Realizer realizer;
    private AbstractStandardNgramModel ngramScorer; 
        
    protected void Initialize(String grammarfile) throws IOException {
    	  URL grammarURL = new File(grammarfile).toURI().toURL();
          System.out.println("Loading grammar from URL: " + grammarURL);
          this.grammar = new Grammar(grammarURL);
          this.realizer = new Realizer(this.grammar);
    }
    
    protected void setLM(boolean useACTR, String modelFile) throws IOException {
    	this.ngramScorer = useACTR ? new ACTRNgramModel(4, modelFile) : new StandardNgramModel(4, modelFile);     
        System.out.println(ngramScorer.getClass());   
   }
   private List<Element> prepareInput(String inputfile) throws IOException {
    	 Document doc = grammar.loadFromXml(inputfile);
    	 Element root = doc.getRootElement();
    	 @SuppressWarnings("unchecked")
 		 List<Element> items = root.getChildren("item");
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
         return items;
    }
    //extracts the targets from the xml sheet
    protected String[] getGoals(List<Element> items) {
    	String[] goals = new String[items.size()];
        for (int i = 0; i < items.size(); i++) {
        	goals[i] = items.get(i).getAttribute("string").getValue();
        }
        return goals;
    }
    
	public void realize(String inputfile, String outputfile) throws Exception {              
		out = new PrintWriter(new BufferedWriter(new FileWriter(outputfile)));
		out.flush();
                    
        List<Element> items = this.prepareInput(inputfile);
        String[] goals = this.getGoals(items);
        
        for (int i = 0; i < items.size(); i++) {
        	Element item = items.get(i);
        	
        	Element lfelt = item.getChild("lf");
	        LF lf = Realizer.getLfFromElt(lfelt);
	        
	        //out.println("Input LF: " + lf);
	        //out.println("Goal: " + goals[i]);

	        realizer.realize(lf, ngramScorer);	        
	        Chart chart = realizer.getChart();
	        ngramScorer.updateAfterRealization(goals[i]);
	        
	        out.println(chart.getBestEdgeAsText());
	        out.flush();
	    }
    }
}
