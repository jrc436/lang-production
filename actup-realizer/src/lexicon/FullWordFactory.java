package lexicon;

import java.util.ArrayList;
import java.util.List;

import util.Pair;

public class FullWordFactory implements IWordFactory {
//    // looks up the word equivalent to w, or if none, returns a new one based on it
    private Word getOrCreateFromW(FullWord w) {
        if (w.isSurfaceWord() && w.attrValPairs == null) {
            return w.pitchAccent == null ? new SimpleWord(w.form) : new WordWithPitchAccent(w.form, w.pitchAccent);
        }
        else {
        	return new FullWord(w.form, w.pitchAccent, w.attrValPairs, w.stem, w.POS, w.supertag, w.semClass);
        }
    }
    
    public Word create(String form) {
        return create(form, null, null, null, null, null, null);
    }
    
    /** Creates a (surface or full) word with the given normalized attribute name and value.
        The attribute names Tokenizer.WORD_ATTR, ..., Tokenizer.SEM_CLASS_ATTR 
        may be used for the form, ..., semantic class. */
    public Word create(String attr, String val) {
        String form = null; String pitchAccent = null;
        List<Pair<String,String>> attrValPairs = null; 
        String stem = null; String POS = null; String supertag = null; String semClass = null;
        if (attr == Tokenizer.WORD_ATTR) form = val;
        else if (attr == Tokenizer.PITCH_ACCENT_ATTR) pitchAccent = val;
        else if (attr == Tokenizer.STEM_ATTR) stem = val;
        else if (attr == Tokenizer.POS_ATTR) POS = val;
        else if (attr == Tokenizer.SUPERTAG_ATTR) supertag = val;
        else if (attr == Tokenizer.SEM_CLASS_ATTR) semClass = val;
        else {
            attrValPairs = new ArrayList<Pair<String,String>>(1);
            attrValPairs.add(new Pair<String,String>(attr, val));
        }
        return create(form, pitchAccent, attrValPairs, stem, POS, supertag, semClass);
    }
    
//    /** Creates a (surface or full) word from the given canonical factors. */
    public Word create(String form, String pitchAccent, List<Pair<String,String>> attrValPairs, String stem, String POS, String supertag, String semClass) {
    	FullWord w = new FullWord(form, pitchAccent, attrValPairs, stem, POS, supertag, semClass);
        return getOrCreateFromW(w);
    }
}
