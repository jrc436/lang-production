package lexicon;

import java.util.List;

import util.Interner;
import util.Pair;
import util.TrieMap;

public class FactorChainFactory implements IWordFactory {

    /** Trie map for canonical instances. */
    protected TrieMap<Object,FactorChainWord> factorChainRoot = new TrieMap<Object,FactorChainWord>(null);
    
    /** Creates a surface word with the given interned form. */
    public Word create(String form) {
        return create(factorChainRoot, Tokenizer.WORD_ATTR, form);
    }
    
    /** Creates a (surface or full) word with the given normalized attribute name and value.
        The attribute names Tokenizer.WORD_ATTR, ..., Tokenizer.SEM_CLASS_ATTR 
        may be used for the form, ..., semantic class. */
    public Word create(String attr, String val) {
        return create(factorChainRoot, attr, val);
    }
    
    private final Interner<Object> intern;
    public FactorChainFactory(Interner<Object> intern) {
    	this.intern = intern;
    }
   
    
    /** Creates a (surface or full) word from the given normalized factors. 
        Returns null if no non-null vals. */
    public Word create(Interner<Object> in,
        String form, String pitchAccent, List<Pair<String,String>> attrValPairs, 
        String stem, String POS, String supertag, String semClass 
    ) {
        // adds non-null vals from the root, in a rough specificity order 
        TrieMap<Object,FactorChainWord> currentNode = factorChainRoot;
        if (POS != null) currentNode = findChild(currentNode, Tokenizer.POS_ATTR, POS);
        if (supertag != null) currentNode = findChild(currentNode, Tokenizer.SUPERTAG_ATTR, supertag);
        if (semClass != null) currentNode = findChild(currentNode, Tokenizer.SEM_CLASS_ATTR, semClass);
        if (stem != null) currentNode = findChild(currentNode, Tokenizer.STEM_ATTR, stem);
        if (form != null) currentNode = findChild(currentNode, Tokenizer.WORD_ATTR, form);
        if (pitchAccent != null) currentNode = findChild(currentNode, Tokenizer.PITCH_ACCENT_ATTR, pitchAccent);
        if (attrValPairs != null) {
            for (int i = 0; i < attrValPairs.size(); i++) {
            	Pair<String,String> p = attrValPairs.get(i);
                String attr = p.a;
                String val = p.b; 
                currentNode = findChild(currentNode, attr, val);
            }
        }
        return currentNode.data;
    }
    public Word create(String form, String pitchAccent, List<Pair<String,String>> attrValPairs, String stem, String POS, String supertag, String semClass) {
    	return create(null, form, pitchAccent, attrValPairs, stem, POS, supertag, semClass);
    }
    
    /** Creates a word from the given node, adding the given interned attr 
        and non-null val. */
    protected Word create(TrieMap<Object,FactorChainWord> currentNode, String attr, String val) {
        TrieMap<Object,FactorChainWord> child = findChild(currentNode, attr, val);
        return child.data;
    }
    
    /** Gets or makes a child node from the given node. */
    protected TrieMap<Object,FactorChainWord> findChild(TrieMap<Object,FactorChainWord> currentNode, String attr, String val) {
        Object key = FactorKey.getKey(this.intern, attr, val);
        TrieMap<Object,FactorChainWord> child = currentNode.findChild(key);
        if (child.data == null) {
            FactorChainWord parent = currentNode.data;
            child.data = new FactorChainWord(key, parent);
        }
        return child;
    }
}
