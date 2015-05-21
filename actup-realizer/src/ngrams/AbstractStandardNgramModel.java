/*
 * $Id: AbstractStandardNgramModel.java,v 1.3 2009/12/21 03:27:18 mwhite14850 Exp $ 
 */
package ngrams;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import lexicon.IWordFactory;
import lexicon.Tokenizer;
import lexicon.Word;
import util.Pair;


/**
 * Abstract class for shared methods used by all standard ngram models.
 * Adapted from the original StandardNgramModel class. 
 * 
 * @author <a href="http://www.ling.osu.edu/~scott/">Scott Martin</a>
 * @version $Revision: 1.3 $
 * @since 0.9.2
 */
public abstract class AbstractStandardNgramModel extends NgramScorer {
	protected boolean openVocab;
	protected double[] params; //this is only because the superclass needs it... the package should not access this... java doesn't have such a setting
	public boolean varsEquiv(double[] otherParams) {
		if (params.length != otherParams.length) {
			return false;
		}
		for (int i = 0; i < params.length; i++) {
			if (params[i] != otherParams[i]) {
				return false;
			}
		}
		return true;
	}
	public void resetVars(double[] otherParams) {
		this.params = otherParams;
	}
    /**
     * Creates a new ngram model of the given order.
     * @param order The order of the model.
     * @param useSemClasses Whether this model should use semantic classes.
     * @param t TODO
     * @see NgramScorer#NgramScorer(int, boolean, Tokenizer)
     */
    protected AbstractStandardNgramModel(int order, boolean useSemClasses, IWordFactory wf, Tokenizer t, double[] varValues) {
		super(order, useSemClasses, wf, t, new int[order]);
		this.params = varValues;
	}
    protected AbstractStandardNgramModel(int order, boolean useSemClasses, IWordFactory wf, Tokenizer t) {
    	this(order, useSemClasses, wf, t, new double[0]);
    }
    protected AbstractStandardNgramModel(int order, IWordFactory wf, Tokenizer t) {
      	this(order, false, wf, t);
    }
    /**
     * This allows an ngram model to update its scoring methodology after each realization. Primarily
     * implemented for the ACTR language model
     */
    public void updateAfterRealization(String realized) {
    	
    }
    /**
     * This clears any intermediate data used for a single realization
     */
    public void clean() {
    	
    }
	/**
     * Converts the words in wordsToScore to strings in stringsToScore, before
     * scoring.
     */
    @Override
    protected List<String> getStringsFromWords(List<Word> wordsToScore) {
        List<String> strings = new ArrayList<String>();
        for (int i = 0; i < wordsToScore.size(); i++) {
            Word w = wordsToScore.get(i);
            String s = w.getForm();
            // check for sem class replacement
            String scr = semClassReplacement(w);
            if (scr != null) s = scr;
            // add pitch accent and attrs, if any
            String pitchAccent = w.getPitchAccent();
            Iterator<Pair<String,String>> pairs = w.getAttrValPairs();
            if (pitchAccent != null || pairs.hasNext()) {
                StringBuffer sb = new StringBuffer();
                sb.append(s);
                if (pitchAccent != null) sb.append('_').append(pitchAccent);
                for (; pairs.hasNext(); ) {
                	Pair<String,String> p = pairs.next();
                    sb.append('_').append(p.b);
                }
                s = sb.toString().intern();
            }
            // check for unknown word
            if (openVocab && trieMapRoot.getChild(s) == null)
                s = "<unk>";
            // add key
            strings.add(s);
        }
        return strings;
    }
    
    /**
     * Returns the log prob of the ngram starting at the given index 
     * in wordsToScore and with the given order, with backoff. 
     * (Assumes words in wordsToScore have already been converted to strings in 
     * stringsToScore, via call to prepareToScoreWords.)
     */
    @Override
    protected float logProbFromNgram(List<String> stringsToScore, int i, int order) {
        // skip initial start tag
        if (i == 0 && order == 1 && stringsToScore.get(0) == "<s>") return 0;
        // set keys list
        List<String> keysList = new ArrayList<String>(); 
        for (int j = i; j < i+order; j++) {
            keysList.add(stringsToScore.get(j));
        }
        // calc log prob
        float retval = logProb(keysList, 0, order);;
        return retval;
    }

}
