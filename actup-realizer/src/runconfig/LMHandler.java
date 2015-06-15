package runconfig;

import grammar.Grammar;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

import ngrams.ACTRNgramModel;
import ngrams.AbstractStandardNgramModel;
import ngrams.RandomModel;
import ngrams.StandardNgramModel;
import optimization.VariableSet;
import realize.RealizeMain;

public class LMHandler {
	private final Map<Integer, AbstractStandardNgramModel> cache;
	private final Map<Integer, Boolean> locks;
	public LMHandler(Grammar grammar, ModelType mt, TrainingSet ts, VariableSet initial) throws IOException {
		File[] lms = new File(ts.getLMDirPath()).listFiles();
		cache = prepareLMCache(grammar, mt, initial, lms);
		locks = new HashMap<Integer, Boolean>();
		for (Integer lock : cache.keySet()) {
			locks.put(lock, false);
		}
	}
	public boolean lockAvail(int lock) {
		return !locks.get(lock);
	}	
	public int numLocks() {
		return locks.size();
	}
	public List<Integer> lockList() {
		List<Integer> retval = new ArrayList<Integer>(locks.keySet());
		Collections.sort(retval);
		return retval;
	}
	//returns what lock you acquired, or -1 if none
	//choices refers to all of the locks that you're interested in acquiring
	public synchronized int attemptAcquireLock(Set<Integer> alreadyHad) {
		synchronized(locks) {
			if (alreadyHad.size() == locks.keySet().size()) {
				return RealizeMain.NO_LOCK_REMAINING;
			}
			for (Integer i : locks.keySet()) {
				if (!alreadyHad.contains(i) && !locks.get(i)) {
					locks.put(i, true);
					alreadyHad.add(i);
					return i;
				}
			}
			return RealizeMain.NO_LOCK_AVAIL;
		}
	}
	public synchronized void releaseLock(int curLock) {
		synchronized(locks) {
			locks.put(curLock, false);
		}
	}
	
	public AbstractStandardNgramModel getFreshModel(int lock, double[] vars) {
		AbstractStandardNgramModel m = cache.get(lock);
		m.clean();
	    m.resetVars(vars);
	    return m;
	}
	private static Map<Integer, AbstractStandardNgramModel> prepareLMCache(Grammar grammar, ModelType mt, VariableSet initial, File[] modelFiles) throws IOException {
		Map<Integer, AbstractStandardNgramModel> lmcache = new HashMap<Integer, AbstractStandardNgramModel>();
		for (File fi : modelFiles) {
			String f = fi.getPath().toString();
			int fileNum = InputHandler.parseRunNum(f);
			if (fileNum == -1) { 
				System.err.println(f + ": is not a valid lm file. It needs to have a number."); 
				System.exit(1); 
			}
			lmcache.put(fileNum, loadLM(grammar, mt, initial, f, mt.getOrder()));
		}
		return lmcache;
	}
	private static AbstractStandardNgramModel loadLM(Grammar grammar, ModelType mt, VariableSet v, String modelFile, int order) throws IOException {
		switch (mt) {
			case ACTR:
				return new ACTRNgramModel(v.getDoubleArray(), order, modelFile, grammar.getWordFactory(), grammar.getTokenizer());
			case Ngram:
					return new StandardNgramModel(order, modelFile, grammar.getWordFactory(), grammar.getTokenizer());
			case Random:
				return new RandomModel(order, grammar.getWordFactory(), grammar.getTokenizer());
			default:
				return null;
		}		
	}
	public static int getLMNumFromFileNum(TrainingSet ts, int fileNum) {
		switch (ts) {
			case SWBD10FOLD:
				return fileNum / (ts.numInputFilesUsed()/10);
			case SWBDM1:
				return fileNum;
			default:
				return -1;
			}
	}
	public static int[] getFileSetFromLMNum(TrainingSet ts, int lmNum) {
		switch (ts) {
			case SWBD10FOLD:
				int[] set = new int[ts.numInputFilesUsed() / 10];
				int st = lmNum * (ts.numInputFilesUsed()/10); //the first index 
				for (int i = st, j = 0; i < st+set.length; i++, j++) {
					set[j] = i;
				}
				String check = "LM NUMBER: "+lmNum+" HAS ";
				for (Integer g : set) {
					check += g+",";
				}
				System.err.println(check);
				return set;
			case SWBDM1:
				return new int[] { lmNum };
			default:
				return null;		
		}
	}
}
