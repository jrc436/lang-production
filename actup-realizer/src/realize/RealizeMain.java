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

import grammar.Grammar;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import ngrams.ACTRNgramModel;
import ngrams.AbstractStandardNgramModel;
import ngrams.RandomModel;
import ngrams.StandardNgramModel;
import optimization.VariableSet;

import org.jdom.Document;
import org.jdom.Element;

import runconfig.IOSettings;
import runconfig.RealizationSettings;
import synsem.LF;

public class RealizeMain
{
	private static Object iolock;
	static {
		iolock = new Object();
	}
	
    private Grammar grammar;
    private Realizer realizer;
    private AbstractStandardNgramModel ngramScorer; 
    
    private HashMap<String, List<Element>> cachedInputs;
    private HashMap<String, AbstractStandardNgramModel> cachedScorers;
    
    public RealizeMain(RealizationSettings rs, String grammarfile) {
         System.out.println("Loading grammar from URL: " + grammarfile);
         try {
        	 loadGrammar(grammarfile, rs);
         }
         catch (IOException e) {
        	 e.printStackTrace();
        	 System.err.println("Error loading grammar");
        	 System.exit(1);
         }
         //maintains a mapping of input files to their elements, to save a bit of time (about 1 second per run)!
         cachedInputs = new HashMap<String, List<Element>>();
         //saves more time! maybe ~15 seconds per run!
         cachedScorers = new HashMap<String, AbstractStandardNgramModel>();
    }
    private Document loadInput(String inputfile) throws IOException {
    	Document d = null;
    	synchronized(iolock) {
    		d = grammar.loadFromXml(inputfile);
    	}
    	return d;
    }
    private synchronized void loadGrammar(String grammarFile, RealizationSettings rs) throws IOException {  	
    	synchronized(iolock) {
	    	URL grammarURL = new File(grammarFile).toURI().toURL();
	    	this.grammar = new Grammar(grammarURL);
	        this.realizer = new Realizer(rs, this.grammar);
    	}
    }
    
    private synchronized void loadLM(IOSettings s, VariableSet v, String modelFile, int order) throws IOException {
    	synchronized(iolock) {
	    	switch (s.getModelType()) {
		    	case ACTR:
		    		//if for whatever reason you think it's a good idea to change the order from the natural order, don't do that.
					this.ngramScorer = new ACTRNgramModel(v.getDoubleArray(), order, modelFile, this.grammar.lexicon.tokenizer);
					break;
		    	case Ngram:
		    		this.ngramScorer = new StandardNgramModel(order, modelFile, this.grammar.lexicon.tokenizer);
		    		break;
		    	case Random:
		    		this.ngramScorer = new RandomModel(order, this.grammar.lexicon.tokenizer);
		    		break;
		    	default:
					break;
	    	}
    	}
    }
    
    public void setLM(IOSettings s, VariableSet v, String lmNum) {
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
    	try {
    		loadLM(s, v, modelFile, order);
    	}
    	catch (IOException io) {
    		io.printStackTrace();
    		System.err.println("Error loading LM");
    		System.exit(1);
    	}
    	cachedScorers.put(modelFile, this.ngramScorer); 
    	System.out.println(ngramScorer.getClass());   
   }
    
 
   private List<Element> prepareInput(String inputfile) {
	     if (cachedInputs.containsKey(inputfile)) {
	    	 return cachedInputs.get(inputfile);
	     }
	     Document doc = null;
	     try {
	    	 doc = loadInput(inputfile);
	     }
	     catch (IOException io) {
	    	 io.printStackTrace();
    		System.err.println("Error loading input file");
    		System.exit(1);
	     }
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
    
    private synchronized FileWriter fw(String outputfile) throws IOException {
    	return new FileWriter(outputfile);
    }
    
	public Realization[] realize(String inputfile, String outputfile) {              
		FileWriter out = null;
		if (outputfile != null) {
			try {				
				out = fw(outputfile);
				out.flush();
			}
			catch (IOException io) {
				io.printStackTrace();
	    		System.err.println("Error initializing output file");
	    		System.exit(1);
			}
		}
         
        List<Element> items = this.prepareInput(inputfile);
        String[] goals = this.getGoals(items);  
        
        Realization[] r = new Realization[goals.length];
        
        for (int i = 0; i < items.size(); i++) {
        	Element item = items.get(i);
        	
        	Element lfelt = item.getChild("lf");
	        LF lf = realizer.getLfFromElt(lfelt);
	        
	        realizer.realize(lf, ngramScorer);	        
	        Chart chart = realizer.getChart();
	        
	        r[i] = chart.getBestRealization(goals[i]);      
	        
	        if (out != null) {
	        	try {
		        	out.write(r[i]+"\n");
		        	out.flush();
	        	}
	        	catch (IOException io) {
	        		io.printStackTrace();
		    		System.err.println("Error writing to output file");
		    		System.exit(1);
	        	}
	        }
	        else {
	        	System.out.println(r[i]);
	        }
	    
	        ngramScorer.updateAfterRealization(goals[i]);
        }
        if (out != null) {
        	try {
        		out.close();
        	}
        	catch (IOException io) {
        		io.printStackTrace();
	    		System.err.println("Error closing output");
	    		System.exit(1);
        	}
        }
        return r;
    }
}
