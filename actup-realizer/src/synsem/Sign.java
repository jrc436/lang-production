///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-9 Jason Baldridge, University of Edinburgh and Michael White
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

package synsem;

import grammar.Grammar;
import grammar.Rule;
import grammar.RuleGroup;
import grammar.TypeChangingRule;
import hylo.HyloHelper;
import hylo.Nominal;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import lexicon.Word;

import org.jdom.Document;
import org.jdom.Element;

import util.Pair;

/**
 * A CCG sign, consisting of a list of words paired with a category.
 * Signs may contain arbitrary data objects which are ignored in equality checking.
 * Non-serializable data objects are filtered during serialization.
 *
 * @author      Jason Baldridge
 * @author      Michael White
 * @version     $Revision: 1.44 $, $Date: 2011/08/27 19:27:01 $
 */
public class Sign implements LexSemOrigin, Serializable, Comparable<Sign> {
    
	private static final long serialVersionUID = 1072712272514007274L;

	/** The words. */
    protected List<Word> _words;
    
    /** The category. */
    protected Category _cat;
    
    /** The derivation history. */
    protected DerivationHistory _history;
    
    /** The lexical head. */
    protected Sign _lexHead;
    
    
    /** List of transient data objects, for retrieval by class. */
    protected LinkedList<Object> data = null; 
    
    protected final Grammar grammar;
    

    /** Constructor for subclasses. */
    protected Sign(Grammar grammar) {
    	
    	this.grammar = grammar;
    }
    
    /** Constructor with derivation history. */
    @SuppressWarnings("unchecked")
	protected Sign(Grammar grammar, List<Word> words, Category cat, DerivationHistory dh, Sign lexHead) {
        this(grammar);
    	_words = (List<Word>) grammar.getIntern().intern(words); 
        _cat = cat;
        _history = dh;
        _lexHead = lexHead;
    }

    /** Constructor with no additional derivation history. */
    public Sign(Grammar grammar, List<Word> words, Category cat) {
        this(grammar, words, cat, null, null);
        _history = new DerivationHistory(grammar, this);
        _lexHead = this;
    }

    /** Constructor with no additional derivation history. */
    public Sign(Grammar grammar, Word word, Category cat) {
    	this(grammar, new ArrayList<Word>(Arrays.asList(word)), cat);
    }
    
    
    // during deserialization, interns words
    @SuppressWarnings("unchecked")
	private void readObject(java.io.ObjectInputStream in) throws IOException, ClassNotFoundException {
    	in.defaultReadObject();
        _words = (List<Word>) grammar.getIntern().intern(_words); 
    }
    
    // during serialization, skips non-serializable data objects
    private void writeObject(java.io.ObjectOutputStream stream) throws IOException {
    	// save old data objects
    	LinkedList<Object> tmp = data;
    	// filter non-serializable ones
    	if (tmp != null) {
    		data = new LinkedList<Object>();
    		for (Object obj : tmp) {
    			if (obj instanceof Serializable) data.add(obj);
    		}
    		if (data.isEmpty()) data = null;
    	}
    	// serialize
    	stream.defaultWriteObject();
    	// restore old data objects
    	data = tmp;
    }

    
    /** Factory method for creating a sign from a lexical sign plus a coarticulation one. */
    public static Sign createCoartSign(Grammar grammar, Category cat, Sign lexSign, Sign coartSign) {
    	
        List<Word> words = lexSign.getWords();
        if (words.size() > 1) 
            throw new RuntimeException("Can't create coarticulation sign from multiple words.");
        Word word = words.get(0);
        Word coartWord = coartSign.getWords().get(0);
        Word wordPlus = Word.createWordWithAttrs(grammar.getWordFactory(), word, coartWord);
        Sign retval = new Sign(grammar, new ArrayList<Word>(Arrays.asList(wordPlus)), cat, null, null);
        retval._lexHead = retval;
        Rule coartRule = new Rule() {
            public String name() { return "coart"; }
            public int arity() { return 1; }
            public List<Category> applyRule(Category[] inputs) { throw new RuntimeException("Not supported."); }
            public RuleGroup getRuleGroup() { throw new RuntimeException("Not supported."); }
            public void setRuleGroup(RuleGroup ruleGroup) { throw new RuntimeException("Not supported."); }
        };
        retval._history = new DerivationHistory(grammar, new Sign[]{lexSign,coartSign}, retval, coartRule);
        return retval;
    }
    
    /** Factory method for creating derived signs with the given cat from the given inputs, rule and lex head. */
    public static Sign createDerivedSign(Grammar grammar, Category cat, Sign[] inputs, Rule rule, Sign lexHead) {
    	
        return new Sign(grammar, cat, inputs, rule, lexHead);
    }

    /** Factory method for creating derived signs from the given result cat, inputs, rule and lex head, 
        with a new LF constructed from the inputs.
        Note that unlike with rule applications, the result LF is constructed with 
        no var substitutions, so it is useful only for creating alternative signs during realization. */
    public static Sign createDerivedSignWithNewLF(Grammar grammar, Category cat, Sign[] inputs, Rule rule, Sign lexHead) {
    	
        Category copyCat = cat.shallowCopy();
        LF lf = null;
        for (int i = 0; i < inputs.length; i++) {
            lf = HyloHelper.append(grammar, lf, inputs[i].getCategory().getLF());
        }
        if (rule instanceof TypeChangingRule) {
            TypeChangingRule tcr = (TypeChangingRule) rule;
            lf = HyloHelper.append(grammar, lf, tcr.getResult().getLF());
        }
        if (lf != null) { HyloHelper.sort(grammar, lf); }
        copyCat.setLF(lf);
        return new Sign(grammar, copyCat, inputs, rule, lexHead);
    }
        
    /** Constructor with words and derivation history formed from the given inputs, rule and lex head. */
    protected Sign(Grammar grammar, Category cat, Sign[] inputs, Rule rule, Sign lexHead) {
        this(grammar, getRemainingWords(inputs, 0), cat, null, lexHead);
        _history = new DerivationHistory(grammar, inputs, this, rule);
    }
    
    // returns the remaining words in a structure sharing way
    private static List<Word> getRemainingWords(Sign[] inputs, int index) {
        if (index == (inputs.length - 1)) {
        	return inputs[index]._words;
        }
        ArrayList<Word> retval = new ArrayList<Word>();
        retval.addAll(inputs[index]._words);
        retval.addAll(getRemainingWords(inputs, index+1));
        return retval;
    }

    
    /** Returns the words of the sign. */
    public List<Word> getWords() {
        return _words;
    }

    /** Returns the words as a string.  Delegates to the current tokenizer's getOrthography method. */
    public String getOrthography() {
        return grammar.getLexicon().getTokenizer().getOrthography(_words);
    }

    /** Returns the sign's category. */
    public Category getCategory() {
        return _cat;
    }

    /** Returns whether the sign is lexical. */
    public boolean isLexical() { return _history.isEmpty(); }
    
    /** Sets the derivation history. */
    public void setDerivationHistory(DerivationHistory dh) {
        _history = dh;
    }
    
    /** Returns the derivation history. */
    public DerivationHistory getDerivationHistory() {
        return _history;
    }

    /** Returns the lexical head. */
    public Sign getLexHead() { return _lexHead; }
    
    
    /** Returns a hash code for this sign. */ 
    public int hashCode() {
        return System.identityHashCode(_words) + _cat.hashCode();
    }
    
    /** Returns whether this sign equals the given object. */
    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (!(obj instanceof Sign)) return false;
        Sign sign = (Sign) obj;
        return _words == sign._words && _cat.equals(sign._cat);
    }
    
    /** 
     * Returns a hash code for this sign with the words restricted to surface words, 
     * and with the LF ignored according to the given flag; 
     * with lexical signs, however, the original hash code is returned, so that 
     * words with signs that differ just in their pos tags can be distinguished 
     * (for robustness).
     */ 
    public int surfaceWordHashCode(boolean ignoreLF) {
        // original hash code for lex signs
        if (_history.getInputs() == null) return hashCode();
        // otherwise use surface words
        int hc = 1;
        for (int i = 0; i < _words.size(); i++) {
            Word word = _words.get(i);
            hc = 31*hc + word.surfaceWordHashCode();
        }
        hc += (ignoreLF) ? _cat.hashCodeNoLF() : _cat.hashCode();
        return hc;
    }
    /** 
     * Returns whether this sign and the given object have equal categories and
     * restrictions to surface words; 
     * with lexical signs, however, the original equals result is returned, so that 
     * words with signs that differ just in their pos tags can be distinguished 
     * (for robustness).
     */
    public boolean surfaceWordEquals(Object obj) {
    	return surfaceWordEquals(obj, false);
    }

    /** 
     * Returns whether this sign and the given object have equal categories and
     * restrictions to surface words,  
     * with the LF ignored according to the given flag; 
     * with lexical signs, however, the original equals result is returned, so that 
     * words with signs that differ just in their pos tags can be distinguished 
     * (for robustness).
     */
    public boolean surfaceWordEquals(Object obj, boolean ignoreLF) {
        if (obj == this) return true;
        if (!(obj instanceof Sign)) return false;
        Sign sign = (Sign) obj;
        // original equals for lex signs
        if (_history.getInputs() == null || sign._history.getInputs() == null) 
            return equals(sign);
        // otherwise use surface words
        if (_words.size() != sign._words.size()) return false;
        for (int i = 0; i < _words.size(); i++) {
            Word word = _words.get(i); 
            Word signWord = (Word) sign._words.get(i);
            if (!word.surfaceWordEquals(signWord)) return false;
        }
        return (ignoreLF) ? _cat.equalsNoLF(sign._cat) : _cat.equals(sign._cat);
    }
    /** 
     * Returns a hash code for this sign with the words restricted to surface words; 
     * with lexical signs, however, the original hash code is returned, so that 
     * words with signs that differ just in their pos tags can be distinguished 
     * (for robustness).
     */ 
    public int surfaceWordHashCode() {
    	return this.surfaceWordHashCode(false);
    }
    
    /** Returns 'orthography :- category'. */
    public String toString() {
        return getOrthography() + " :- " + _cat.toString(); // for lex head: + " --> " + _lexHead.getWordForm();
    }
 
    
    /** 
     * Returns the words in an XML doc, with no labeled spans for nominals. 
     */
    public Document getWordsInXml() {
    	Set<Nominal> emptySet = Collections.emptySet();
    	return getWordsInXml(emptySet); 
    }
    
    /** 
     * Returns the words in an XML doc, with labeled spans for the given nominals, 
     * and with pitch accents and boundary tones converted to elements. 
     * Each orthographic word appears in a separate element, 
     * with multiwords grouped under a multiword element.
     * Attribute-value pairs for the word (if any) appear on the word 
     * or multiword element.
     * Words are also expanded using the grammar's tokenizer.
     */
    public Document getWordsInXml(Set<Nominal> nominals) {
        Map<Nominal, Integer> nominalsMap = new LinkedHashMap<Nominal, Integer>(); 
        setMaxOrthLengths(nominals, nominalsMap);
        Document doc = new Document();
        Element root = new Element("seg");
        doc.setRootElement(root);
        addWordsToXml(root, nominalsMap);
        return doc;
    }
    
    // finds the maximum orthography lengths for signs headed by the given nominals
    private void setMaxOrthLengths(Set<Nominal> nominals, Map<Nominal, Integer> nominalsMap) {
        // update map
        Nominal index = _cat.getIndexNominal();
        if (index != null && nominals.contains(index)) {
            int orthLen = getOrthography().length();
            if (!nominalsMap.containsKey(index) || orthLen > nominalsMap.get(index)) {
                nominalsMap.put(index, orthLen);
            }
        }
        // recurse
        Sign[] inputs = _history.getInputs();
        if (inputs == null) return;
        for (int i = 0; i < inputs.length; i++) {
            inputs[i].setMaxOrthLengths(nominals, nominalsMap); 
        }
    }
    
    // recursively adds orthographic words as XML to the given parent, 
    // using the nominals map to determine labeled spans
    private void addWordsToXml(Element parent, Map<Nominal, Integer> nominalsMap) {
        // check for matching nominal as index of target cat; 
        // if found, update parent to labeled span element
        Nominal index = _cat.getIndexNominal();
        if (index != null && nominalsMap.containsKey(index) && 
            nominalsMap.get(index) == getOrthography().length()) 
        {
            // remove index key from map, to avoid duplicate spans with the same length
            nominalsMap.remove(index);
            // make span element, update parent
            Element span = new Element("span");
            span.setAttribute("label", index.toString());
            parent.addContent(span);
            parent = span;
        }
        // process inputs from derivation history
        Sign[] inputs = _history.getInputs();
        if (inputs == null) {
            // in leaf case, word list must be a singleton
            Word word = _words.get(0); 
            // check for boundary tone
            if (Grammar.isBoundaryTone(word.getForm())) {
                // add element for boundary tone
                Element boundary = new Element("boundary");
                boundary.setAttribute("type", word.getForm());
                parent.addContent(boundary);
                return;
            }
            // check for pitch accent
            if (word.getPitchAccent() != null) {
                // add pitchaccent element containing word(s) with corresponding accent
                Element pitchaccent = new Element("pitchaccent");
                pitchaccent.setAttribute("type", word.getPitchAccent());
                addWords(pitchaccent, word);
                parent.addContent(pitchaccent);
                return;
            }
            // otherwise add word(s)
            addWords(parent, word);
            return;
        }
        if (inputs.length == 1) {
            inputs[0].addWordsToXml(parent, nominalsMap);
            return;
        }
        for (int i = 0; i < inputs.length; i++) {
            inputs[i].addWordsToXml(parent, nominalsMap);
        }
    }
    
    // adds one or more word elements after expanding surface form; 
    // multiwords are enclosed within a multiword element; 
    // any attribute-value pairs are added to the word or multiword element
    private void addWords(Element parent, Word word) {
        List<String> orthWords = grammar.getLexicon().getTokenizer().expandWord(word);
        Element child;
        if (orthWords.size() == 1) {
            Element wordElt = new Element("word");
            wordElt.addContent(orthWords.get(0));
            child = wordElt;
        }
        else {
            Element multiwordElt = new Element("multiword");
            for (int i = 0; i < orthWords.size(); i++) {
                Element wordElt = new Element("word");
                wordElt.addContent(orthWords.get(i));
                multiwordElt.addContent(wordElt);
            }
            child = multiwordElt;
        }
        for (Iterator<Pair<String,String>> it = word.getAttrValPairs(); it.hasNext(); ) {
            Pair<String,String> p = it.next();
            String attr = p.a; String val = p.b;
            child.setAttribute(attr, val);
        }
        parent.addContent(child);
    }
    

    /**
     * Returns a string showing the bracketings implied by the derivation.
     * See DerivationHistory.toString to see the complete derivation in 
     * vertical list form.
     */
    public String getBracketedString() {
        Sign[] inputs = _history.getInputs();
        if (inputs == null) return getOrthography();
        if (inputs.length == 1) return inputs[0].getBracketedString();
        StringBuffer sb = new StringBuffer();
        sb.append("(");
        for (int i = 0; i < inputs.length; i++) {
            sb.append(inputs[i].getBracketedString());
            if (i < (inputs.length - 1)) sb.append(" ");
        }
        sb.append(")");
        return sb.toString();
    }
    
    /**
     * Returns the category's supertag.
     */
    public String getSupertag() { return _cat.getSupertag(); }
    
    /**
     * Returns the word form of the first word. 
     */
    public String getWordForm() { return _words.get(0).getForm(); }
    
    /**
     * Returns the POS tag of the first word.
     */
    public String getPOS() { return _words.get(0).getPOS(); }
    
    /**
     * Sets the origin of the elementary predications.
     */
    public void setOrigin() { HyloHelper.setOrigin(_cat.getLF(), this); }
    
    /**
     * Returns the index of the first word of the given lex sign in this sign's 
     * list of words, or -1 if the given lex sign is not in this sign's derivation 
     * history.
     */
    public int wordIndex(Sign lexSign) {
    	return wordIndex(lexSign, new int[]{0});
    }
    
    // returns word index relative to input offset
    private int wordIndex(Sign lexSign, int[] offset) {
    	if (this == lexSign) return offset[0];
    	if (isLexical()) {
    		offset[0] += _words.size();
    		return -1;
    	}
        Sign[] inputs = _history.getInputs();
        for (int i = 0; i < inputs.length; i++) {
        	int retval = inputs[i].wordIndex(lexSign, offset);
        	if (retval >= 0) return retval;
        }
        return -1;
    }
    
    
    /** Adds a data object to the front of the list of data objects. */
    public void addData(Object obj) {
    	if (data == null) data = new LinkedList<Object>();
    	data.addFirst(obj);
    }
    
    /** Returns the first data object with the given class, or null if none. */
    public Object getData(Class<?> objClass) {
    	if (data == null) return null;
    	for (Object obj : data) {
    		if (obj.getClass() == objClass) return obj;
    	}
    	return null;
    }
	
    /** Tests serialization of simple types, including resolution. */
    public void debugSerialization() throws IOException, ClassNotFoundException {
        // test serialization
    	String filename = "tmp.ser";
    	ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(filename));
    	System.out.println("Writing this: " + this);
    	System.out.println(this.getDerivationHistory());
    	out.writeObject(this);
    	out.close();
    	ObjectInputStream in = new ObjectInputStream(new FileInputStream(filename));
    	System.out.print("Reading sign: ");
    	Sign sign = (Sign) in.readObject();
    	System.out.println(sign);
    	System.out.println(sign.getDerivationHistory());
    	in.close();
    	// test identity and equality
    	System.out.println("this == sign?: " + (this == sign));
    	System.out.println("this.equals(sign)?: " + (this.equals(sign)));
    }

	@Override
	public int compareTo(Sign o) {
		int cmp = 0;
    	cmp = this.getDerivationHistory().compareTo(o.getDerivationHistory());
    	if (cmp != 0) return cmp;
    	List<Word> words1 = this.getWords(); 
    	List<Word> words2 = o.getWords();
    	cmp = compareTo(words1, words2);
    	if (cmp != 0) return cmp;
    	// TODO: implement compareTo method on categories
    	int h1 = this.getCategory().hashCode();
    	int h2 = o.getCategory().hashCode();
    	if (h1 < h2) return -1;
    	if (h1 > h2) return 1;
    	return 0;
	}
	private int compareTo(List<Word> words1, List<Word> words2) {
    	int i=0;
    	while (i < words1.size() || i < words2.size()) {
    		if (i == words1.size()) return -1;
    		if (i == words2.size()) return 1;
    		Word w1 = words1.get(i); Word w2 = words2.get(i);
    		int cmp = w1.compareTo(w2);
    		if (cmp != 0) return cmp;
    		i++;
    	}
    	return 0;
    }
}
