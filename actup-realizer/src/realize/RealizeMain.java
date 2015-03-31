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

package realize;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import runconfig.Settings;
import grammar.Grammar;
import ngrams.ACTRNgramModel;
import ngrams.AbstractStandardNgramModel;
import ngrams.RandomModel;
import ngrams.StandardNgramModel;
import optimization.VariableSet;
import synsem.LF;

import org.jdom.Document;
import org.jdom.Element;

public class RealizeMain
{
    private Grammar grammar;
    private Realizer realizer;
    private AbstractStandardNgramModel ngramScorer; 
    
    //while this could theoretically be static, the consequences of doing that could be concurrent access, which is bad.
    private HashMap<String, List<Element>> cachedInputs;
    private HashMap<String, AbstractStandardNgramModel> cachedScorers;
    
    public RealizeMain(String grammarfile) throws IOException {
    	 URL grammarURL = new File(grammarfile).toURI().toURL();
         System.out.println("Loading grammar from URL: " + grammarURL);
         this.grammar = new Grammar(grammarURL);
         this.realizer = new Realizer(this.grammar);
         
         //maintains a mapping of input files to their elements, to save a bit of time (about 1 second per run)!
         cachedInputs = new HashMap<String, List<Element>>();
         //saves more time! maybe ~15 seconds per run!
         cachedScorers = new HashMap<String, AbstractStandardNgramModel>();
    }
    public void setLM(Settings s, VariableSet v, String lmNum) throws IOException {
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
    		AbstractStandardNgramModel score = cachedScorers.get(modelFile); //need to fix this later 
    		score.resetVars(v.getDoubleArray());
    		this.ngramScorer = score;
    		ngramScorer.clean();
    		return;
    	}
    	int order = s.getModelType().getOrder();
    	switch (s.getModelType()) {
	    	case ACTR:
	    		//if for whatever reason you think it's a good idea to change the order from the natural order, don't do that.
				this.ngramScorer = new ACTRNgramModel(v.getDoubleArray(), order, modelFile);
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
    	//cachedScorers.put(modelFile, this.ngramScorer); need to fix this later
    	System.out.println(ngramScorer.getClass());   
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
