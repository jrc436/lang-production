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

import java.io.FileWriter;
import java.io.IOException;
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
import unify.UnifyControl;

public class RealizeMain
{
	private static Object iolock;
	static {
		iolock = new Object();
	}

	private final String grammarfile;
	private final RealizationSettings rs;
	private final boolean useCache;
	//private AbstractStandardNgramModel ngramScorer; 

	private HashMap<String, AbstractStandardNgramModel> cachedScorers;
	private HashMap<String, InputStruct[]> cachedInputs;
	private HashMap<String, Boolean> locks;


	//should only need one realizemain, generally, as it is generally threadsafe
	//with the locking mechanisms in valleyclimber
	public RealizeMain(RealizationSettings rs, String grammarfile, boolean useCache) {
		this.rs = rs;
		this.grammarfile = grammarfile;
		this.useCache = useCache;
		System.out.println("Loading grammar from URL: " + grammarfile);
		
		
			
		//maintains a mapping of input files to their elements, to save a bit of time (about 1 second per run)!
		cachedInputs = new HashMap<String, InputStruct[]>();
		//saves more time! maybe ~15 seconds per run!
		cachedScorers = new HashMap<String, AbstractStandardNgramModel>();
		locks = new HashMap<String, Boolean>();
	}
	private Document loadInput(Grammar grammar, String inputfile) throws IOException {
		Document d = null;
		synchronized(iolock) {
			d = grammar.loadFromXml(inputfile);
		}
		return d;
	}

	private synchronized Grammar loadGrammar() {  	
		synchronized(iolock) {
			try {
				return new Grammar(grammarfile, new UnifyControl());
			}
			catch (IOException e) {
				e.printStackTrace();
				System.err.println("Error loading grammar");
				System.exit(1);
			}
			return null;
		}
	}
    
	private AbstractStandardNgramModel loadLM(Grammar grammar, IOSettings s, VariableSet v, String modelFile, int order) throws IOException {
		synchronized(iolock) {
			switch (s.getModelType()) {
				case ACTR:
					return new ACTRNgramModel(v.getDoubleArray(), order, modelFile, grammar);
				case Ngram:
						return new StandardNgramModel(order, modelFile, grammar);
				case Random:
					return new RandomModel(order, grammar);
				default:
					return null;
			}
		}
	}

	public AbstractStandardNgramModel setLM(Grammar grammar, IOSettings s, VariableSet v, String lmNum) {
		String modelFile = s.getTrainingSet().getLMDirPath();
		AbstractStandardNgramModel score = null;
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
		if (useCache) {
			synchronized(cachedScorers) {
				if (cachedScorers.containsKey(modelFile)) {
					System.out.println("Loading scorer from cache");
					score = cachedScorers.get(modelFile); 
					score.resetVars(v.getDoubleArray());
					score.clean();
					return score;
				}
			}
		}
		int order = s.getModelType().getOrder();
		try {
			score = loadLM(grammar, s, v, modelFile, order);
		}
		catch (IOException io) {
			io.printStackTrace();
			System.err.println("Error loading LM");
			System.exit(1);
		}
		if (useCache) {
			synchronized(cachedScorers) {
				cachedScorers.put(modelFile, score);
			}
		}
		System.out.println(score.getClass());
		return score;
	}
	private InputStruct[] getInputStruct(Grammar grammar, String inputfile) {
		if (useCache) {
			synchronized(cachedInputs) {
				if (cachedInputs.containsKey(inputfile)) {
					System.out.println("Loading input from cache");
					return cachedInputs.get(inputfile);
				}
			}
		}
		List<Element> items = prepareInput(grammar, inputfile);
		String[] goals = getGoals(items);
		InputStruct[] inp = new InputStruct[items.size()];
		for (int i = 0; i < items.size(); i++) {
			Element item = items.get(i);
			Element lfelt = item.getChild("lf");
			LF lf = Realizer.getLfFromElt(grammar, lfelt);
			inp[i] = new InputStruct(lf, goals[i]);
		}
		if (useCache) {
			synchronized(cachedInputs) {
				cachedInputs.put(inputfile, inp);
			}
		}
		return inp;
	}

	private List<Element> prepareInput(Grammar grammar, String inputfile) {
		Document doc = null;
		try {
			doc = loadInput(grammar, inputfile);
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
	
	public Realizer createNewRealizer() {
		return new Realizer(this.rs, this.loadGrammar());
	}
	
	//each one of these calls needs its own realizer to make it threadsafe.
	public Realization[] realize(AbstractStandardNgramModel ngramScorer, Realizer realizer, String inputfile, String outputfile) {              
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
		
		InputStruct[] items = this.getInputStruct(realizer.getGrammar(), inputfile);
		Realization[] r = new Realization[items.length];
		
		for (int i = 0; i < items.length; i++) {
			realizer.realize(items[i].lf, ngramScorer);
			Chart chart = realizer.getChart();
			r[i] = chart.getBestRealization(items[i].goal);
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
			ngramScorer.updateAfterRealization(items[i].goal);
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
	public synchronized boolean attemptAcquireLock(String num) {
		//if it doesn't have an entry for it yet, or if the entry is that it's unlocked
		if (!useCache) {
			return true; //this is only to prevent concurrent usage of the same element in the dictionary, mostly because they can be modified!
		}
		if (!locks.containsKey(num) || locks.get(num) == false) {
			locks.put(num, true);
			return true;
		}
		return false;
	}
	public synchronized void releaseLock(String num) {
		if (useCache) {
			locks.put(num, false);	
		}
	}
	//given an input file, we want to be able to get:
	//
	private class InputStruct {
		private final LF lf;
		private final String goal;
		private InputStruct(LF lf, String goal) {
			this.lf = lf;
			this.goal = goal;
		}
	}
}
