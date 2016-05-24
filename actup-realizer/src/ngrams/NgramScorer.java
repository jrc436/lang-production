///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-8 University of Edinburgh (Michael White)
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

package ngrams;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import lexicon.IWordFactory;
import lexicon.Tokenizer;
import lexicon.Word;
import synsem.Sign;
import synsem.SignScorer;
import util.TrieMap;

/**
 * Super class for n-gram scoring models.
 *
 * @author      Michael White
 * @version     $Revision: 1.37 $, $Date: 2010/02/25 22:26:11 $
 */
public abstract class NgramScorer implements SignScorer
{
    
    // tokenizer reference
    protected final IWordFactory wf;
    protected final Tokenizer t;
    protected final boolean useSemClasses;
    protected final boolean useNgramFeatures = true;
    protected final int order;
    
    //should only be used in loading the model, I believe
    protected final int[] numNgrams;   /** The n-gram totals for different histories. */
    
    /** Weak hash map for cached log probs, keyed from a sign's words. */
    protected Map<List<Word>,Float> cachedLogProbs;
    
    /** Root of the n-gram trie.  Nodes store NgramFloats instances. */
    //this is loaded from the model, after which it should NOT be changed!!!
    protected TrieMap<String, NgramFloats> trieMapRoot = new TrieMap<String, NgramFloats>(null);
    
    
	protected NgramScorer(int order, IWordFactory wf, Tokenizer t) {
		this(order, false, wf, t);
	}
	
	protected NgramScorer(int order, boolean useSemClasses, IWordFactory wf, Tokenizer t) {
		this(order, useSemClasses, wf, t, new int[0]);
	}
	protected NgramScorer(int order, boolean useSemClasses, IWordFactory wf, Tokenizer t, int[] numNgrams) {
		this.order = order;
		this.useSemClasses = useSemClasses;
		this.wf = wf;
		this.t = t;
		cachedLogProbs = new WeakHashMap<List<Word>, Float>();
		this.numNgrams = numNgrams;
	}    
    

    /** An ngram data object, for holding the log prob and backoff weight. */
    class NgramFloats {       
    	private float logprob; /** The log prob. */     
    	private float bow;  /** The backoff weight. */
        NgramFloats(float logprob, float bow) {
            this.logprob = logprob; this.bow = bow; 
        }      
        @Override
        public String toString() { return "logprob: " + logprob + ", bow: " + bow; }
    }
    
    /** Gets a cached log prob for the given list of words (or null if none). */
    protected synchronized Float getCachedLogProb(List<Word> words) {
        return cachedLogProbs.get(words);
    }
    
    /** Caches a log prob for the given list of words. */
    protected synchronized void putCachedLogProb(List<Word> words, Float logprob) {
        cachedLogProbs.put(words, logprob);
    }

    
    /** 
     * Returns a score between 0 (worst) and 1 (best) for the given sign and completeness flag, based on the n-gram score of the sign's words.
     * If the sign is complete, sentence delimiters are added before scoring the words, if not already present.
     * Returns 0 if any filter flags the n-gram for filtering, or if the sign has no words.
     * and then returns the result of <code>logprob()</code> converted to a probability.
     */
    public double score(Sign sign, boolean complete) {
    	return convertToProb(logprob(sign, complete));
    }
    
    /** 
     * Returns a log prob for the given sign and completeness flag, based on the n-gram log prob of the sign's words.
     * If the sign is complete, sentence delimiters are added before scoring the words, if not already present.
     * Returns the log prob for zero probability if any filter flags the n-gram for filtering, or if 
     * the sign has no words. then returns the result of <code>logProb()</code>.
     */
    public double logprob(Sign sign, boolean complete) {
        List<Word> words = sign.getWords(); 
        if (words == null) return 0;
        if (!complete) { // check cache
            Float logprob = getCachedLogProb(words);
            if (logprob != null) return logprob;
        }
        boolean tagsAdded = setWordsToScore(words, complete);
        List<String> strings = getStringsFromWords(words);
        double retval = logprob(tagsAdded, strings, sign, words);
        return retval;
    }
    /** 
     * Returns a log prob for the words in wordsToScore.
     * The default method returns the log prob of the word sequence 
     * as determined by this language model's <code>logProbFromNgram</code> method.
     * The probabilities for the first n-1 words are backed off to the  
     * lower order probabilities.
     * If the tagsAdded flag is false, the cache is checked to see whether 
     * the log prob of the words of signToScore's initial sign has already 
     * been calculated, and at the end the log prob of signToScore's words 
     * is stored in the cache.
     */
    protected double logprob(boolean tagsAdded, List<String> strings, Sign signToScore, List<Word> wordsToScore) {
        float logProbTotal = 0;
        int numCached = 0;
        if (!tagsAdded && signToScore != null) { // check cache for initial words
            Sign[] inputs = signToScore.getDerivationHistory().getInputs();
            if (inputs != null) {
                Sign initialSign = inputs[0];
                List<Word> initialWords = initialSign.getWords();
                Float logprob = getCachedLogProb(initialWords);
                if (logprob != null) {
                    logProbTotal = logprob.floatValue();
                    numCached = initialWords.size();
                }
            }
        }
        for (int i = numCached; i < wordsToScore.size(); i++) {
            int orderToUse = Math.min(order, i+1);
            int startPos = i - (orderToUse-1);
            logProbTotal += logProbFromNgram(strings, startPos, orderToUse);
        }
        if (!tagsAdded && signToScore != null) { // add log prob to cache
            putCachedLogProb(signToScore.getWords(), new Float(logProbTotal));
        }
        return logProbTotal;
    }
    
    /**
	 * Resets wordsToScore to the given ones, reversing them when the reverse
	 * flag is true, and adding sentence delimiters if not already present, when
	 * the completeness flag is true. Also sets the tagsAdded flag.
	 */
    protected boolean setWordsToScore(List<Word> words, boolean complete) {
        boolean tagsAdded = false; 
        if (complete && words.get(0).getForm() != "<s>") { 
            words.add(Word.createWord(wf, "<s>"));
            tagsAdded = true;
        }
        if (complete && (words.get(words.size()-1).getForm() != "</s>")) {
            words.add(Word.createWord(wf, "</s>"));
            tagsAdded = true;
        }
        return tagsAdded;
    }
    
    /** Optional step to do further preparation before scoring words. 
     * @return */
    protected List<String> getStringsFromWords(List<Word> words) {
    	return null;
    }
	
    
    
    /**
	 * Returns the log prob of the ngram starting at the given index in
	 * wordsToScore and with the given order, with backoff.
	 */
    abstract protected float logProbFromNgram(List<String> wordsToScore, int i, int order);
    
    /**
	 * Sets the keys in keysList to hold the ngram starting at the given index in
	 * wordsToScore and with the given order; returns true if the operation 
	 * succeeds normally. The default implementation invokes
	 * logProbFromNgram, and returns false if the log prob is zero.
	 */
	protected boolean setKeysToNgram(List<String> wordsToScore, int i, int order) {
		float logprob = logProbFromNgram(wordsToScore, i, order);
		return logprob != 0;
	}
    
    /** Returns whether the given semantic class is a replacement one. */
    protected boolean isReplacementSemClass(String semClass) {
        return semClass != null && t.isReplacementSemClass(semClass);
    }
    
    /**
	 * Returns the semantic class replacement value (the semantic class
	 * uppercased and interned) for the given word, if apropos, otherwise null.
	 */
    protected String semClassReplacement(Word w) {
        if (useSemClasses) {
            String semClass = w.getSemClass();
            if (isReplacementSemClass(semClass)) 
                return semClass.toUpperCase().intern();
        }
        // otherwise null
        return null;
    }
    
    
    /**
	 * Adds the TrieMap children, with their keys, under the given prefix, then
	 * resets the lists.
	 */
    protected void addTrieMapChildren(List<String> prefix, List<String> keys, List<TrieMap<String,NgramFloats>> children) {
        if (!keys.isEmpty()) {
            TrieMap<String, NgramFloats> prefixNode = trieMapRoot.findChildFromList(prefix);
            prefixNode.addChildren(keys, children);
        }
        prefix.clear(); keys.clear(); children.clear();
    }
    
    /** Returns the TrieMap node for the given sublist of keysList. */ 
    protected TrieMap<String, NgramFloats> getNode(List<String> keysList, int pos, int len) {
        return trieMapRoot.getChildFromList(keysList.subList(pos, pos+len));
    }
    
    /**
	 * Returns the log prob (base 10) of the given sublist of keysList, with
	 * backoff, or -99 if not found.
	 */
    protected float logProb(List<String> keysList, int pos, int len) {
        TrieMap<String,NgramFloats> node = getNode(keysList, pos, len);
        if (node != null && node.data != null) {
        	return node.data.logprob;
        }
        if (len == 1) {
        	return -99;
        }
        float retval = logProb(keysList, pos+1, len-1);
      //  if (debugScore) System.out.print("(" + (len-1) + "-gram: " + retval + ") ");
        if (retval > -99) retval += backoffWeight(keysList, pos, len-1);
        return retval;
    }
    
    /**
	 * Returns the back-off weight (log base 10) of the given sublist of
	 * keysList, or 0 if not found.
	 */
    protected float backoffWeight(List<String> keysList, int pos, int len) {
        TrieMap<String,NgramFloats> node = getNode(keysList, pos, len);
        if (node != null && node.data != null) {
            float retval = node.data.bow;
            // if (debugScore && retval != 0) System.out.print("(bow: " + retval + ") ");
            return retval;
        }
        return 0;
    }



	/**
     * Returns the rank order centroid weights for a ranked list of the given length. 
     * The weights go from highest to lowest, and sum to 1.
     */
    // ex:
    // weight 1 0.5208333333333333
    // weight 2 0.2708333333333333
    // weight 3 0.14583333333333331
    // weight 4 0.0625
    public static double[] rankOrderCentroidWeights(int length) {
        double[] retval = new double[length];
        for (int i = 0; i < length; i++) {
            double weight_i = 0;
            for (int j = i; j < length; j++) {
                weight_i += 1 / (double) (j+1);
            }
            weight_i = weight_i / (double) length;
            retval[i] = weight_i;
        }
        return retval;
    }
    
    
    /** Converts a base 10 log prob to an actual probability, checking for -99 (not found). */
    public static double convertToProb(double logProb) {
        if (logProb <= -99) { return 0; }
        else return Math.pow(10, logProb);
    }
    
    /** Converts a probability to a base 10 log prob, returning -99 if zero. */
    public static double convertToLogProb(double prob) {
        if (prob == 0) return -99;
        else return Math.log(prob) / Math.log(10);
    }
    
    /** Converts a base 10 log prob to the corresponding perplexity. */
    public static double convertToPPL(double logProb) {
        return Math.exp(- logProb * Math.log(10));
    }

    
    /** Sets up tokenizer for reading in language models. */ 
    public static StreamTokenizer initTokenizer(Reader in) {
        StreamTokenizer tokenizer = new StreamTokenizer(in);
        tokenizer.resetSyntax();
        tokenizer.wordChars(0,255);
        tokenizer.whitespaceChars(' ',' ');
        tokenizer.whitespaceChars('\t','\t');
        tokenizer.whitespaceChars('\n','\n');
        tokenizer.whitespaceChars('\r','\r');
        tokenizer.eolIsSignificant(true);
        return tokenizer;
    }
    
    /**
	 * Reads a line of up to tokens.length tokens using the given tokenizer,
	 * with the remaining array elements set to null.
	 */
    public static void readLine(StreamTokenizer tokenizer, String[] tokens) throws IOException {
        int index = 0;
        int ttype;
        while ( (ttype = tokenizer.nextToken()) != StreamTokenizer.TT_EOF && ttype != StreamTokenizer.TT_EOL ) {
            if (index < tokens.length && ttype == StreamTokenizer.TT_WORD) { 
                tokens[index] = tokenizer.sval;
                index++;
            }
        }
        for (int i = index; i < tokens.length; i++) {
            tokens[i] = null;
        }
    }
}

