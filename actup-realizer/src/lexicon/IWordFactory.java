package lexicon;

import java.util.List;

import util.Pair;

public interface IWordFactory {
	   /** Creates a surface word. */
    public Word create(String form);
    /** Creates a (surface or full) word with the given normalized attribute name and value.
        The attribute names Tokenizer.WORD_ATTR, ..., Tokenizer.SEM_CLASS_ATTR 
        may be used for the form, ..., semantic class. */
    public Word create(String attr, String val);
    /** Creates a (surface or full) word from the given canonical factors. */
    public Word create(String form, String pitchAccent, List<Pair<String,String>> attrValPairs, String stem, String POS, String supertag, String semClass);

}
