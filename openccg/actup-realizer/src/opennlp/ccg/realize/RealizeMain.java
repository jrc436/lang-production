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

package opennlp.ccg.realize;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import opennlp.ccg.Settings;
import opennlp.ccg.grammar.Grammar;
import opennlp.ccg.ngrams.ACTRNgramModel;
import opennlp.ccg.ngrams.AbstractStandardNgramModel;
import opennlp.ccg.ngrams.RandomModel;
import opennlp.ccg.ngrams.StandardNgramModel;
import opennlp.ccg.optimization.Variable;
import opennlp.ccg.synsem.LF;

import org.jdom.Document;
import org.jdom.Element;

public class RealizeMain
{
    private Grammar grammar;
    private Realizer realizer;
    private AbstractStandardNgramModel ngramScorer; 
    
    private HashMap<String, List<Element>> cachedInputs;
    private HashMap<String, AbstractStandardNgramModel> cachedScorers;
    
    public RealizeMain(String grammarfile) throws IOException {
    	 URL grammarURL = new File(grammarfile).toURI().toURL();
         System.out.println("Loading grammar from URL: " + grammarURL);
         this.grammar = new Grammar(grammarURL);
         this.realizer = new Realizer(this.grammar);
         
         //maintains a mapping of input files to their elements, to save a bit of time (about 1 second per run)!
         cachedInputs = new HashMap<String, List<Element>>();
         cachedScorers = new HashMap<String, AbstractStandardNgramModel>();
    }
    public void setLM(Settings s, Variable[] v, String lmNum) throws IOException {
    	String modelFile = s.getTrainingSet().getLMDirPath();
    	switch (s.getTrainingSet()) {
    		case WSJ:
    			modelFile += "wsj-lm";
    			break;
    		case SWBD:
    			modelFile += ("sw-" + lmNum + ".lm");
    			break;
    		default:
    			modelFile = "";
    			break;
    	}
    	if (cachedScorers.containsKey(modelFile)) {
    		ngramScorer = cachedScorers.get(modelFile);
    		cleanLM();
    		return;
    	}
    	int order = s.getModelType().getOrder();
    	switch (s.getModelType()) {
	    	case ACTR:
				this.ngramScorer = new ACTRNgramModel(v[0].getCurrentValue(), v[1].getCurrentValue(), v[2].getCurrentValue(), (int) Math.round(v[3].getCurrentValue()), order, modelFile);
				break;
	    	case Ngram:
	    		this.ngramScorer = new StandardNgramModel(order, modelFile);
	    		break;
	    	case Random:
	    		this.ngramScorer = new RandomModel(order);
	    		break;
	    	default:
				break;
    	}
    	cachedScorers.put(modelFile, this.ngramScorer);
    	System.out.println(ngramScorer.getClass());   
   }
   protected boolean cleanLM() {
	   if (ngramScorer == null) {
		   return false;
	   }
	   ngramScorer.clean();
	   return true;
   }
   private List<Element> prepareInput(String inputfile) throws IOException {
	     if (cachedInputs.containsKey(inputfile)) {
	    	 return cachedInputs.get(inputfile);
	     }
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
         cachedInputs.put(inputfile, items);
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
    
	public Realization[] realize(String inputfile, String outputfile) throws Exception {              
		PrintWriter out = null;
		if (outputfile != null) {
			out = new PrintWriter(new BufferedWriter(new FileWriter(outputfile)));
			out.flush();
		}
           
        List<Element> items = this.prepareInput(inputfile);
        String[] goals = this.getGoals(items);  
        
        Realization[] r = new Realization[goals.length];
        
        for (int i = 0; i < items.size(); i++) {
        	Element item = items.get(i);
        	
        	Element lfelt = item.getChild("lf");
	        LF lf = Realizer.getLfFromElt(lfelt);

	        realizer.realize(lf, ngramScorer);	        
	        Chart chart = realizer.getChart();
	        
	        r[i] = chart.getBestRealization(goals[i]);
	        
	        if (out != null) {
	        	out.println(r[i]);
	        	out.flush();
	        }
	        else {
	        	System.out.println(r[i]);
	        }
	    
	        ngramScorer.updateAfterRealization(goals[i]);
        }
        if (out != null) {
        	out.close();
        }
        return r;
    }
}
