///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2004-5 University of Edinburgh (Michael White)
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

package lexicon;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import util.Pair;
import util.CollectionContains;

/**
 * A Word object may either be a surface word or a full word.
 * A surface word holds a surface form, an optional pitch accent, and an 
 * optional list of extra attribute-value pairs.
 * A full word additionally contains a stem, part of speech, supertag and semantic class.
 * A word may be a multiword consisting of multiple orthographic words, 
 * typically separated by underscores in the surface form.
 * For efficient storage and equality checking, Word objects are interned by 
 * the factory methods of the configured WordFactory.
 *
 * @author      Michael White
 * @version     $Revision: 1.21 $, $Date: 2009/12/16 22:39:20 $
 */
abstract public class Word implements Serializable, Comparable<Word> {

	private static final long serialVersionUID = 1L;

	/** Returns the surface form. */
    abstract public String getForm();
    
    /** Returns the pitch accent. */
    abstract public String getPitchAccent();
    
    // empty iterator
    private Iterator<Pair<String,String>> emptyIterator = new ArrayList<Pair<String,String>>(0).iterator();

    /** Returns an iterator over the extra attribute-value pairs. */
    public Iterator<Pair<String,String>> getAttrValPairs() { 
        List<Pair<String,String>> pairs = getAttrValPairsList();
        return (pairs != null) ? pairs.iterator() : emptyIterator; 
    } 
    
    /** Returns an iterator over the surface attribute-value pairs, including the pitch accent (if any). */
    public Iterator<Pair<String,String>> getSurfaceAttrValPairs() {
        List<Pair<String,String>> pairs = getAttrValPairsList(); String pitchAccent = getPitchAccent();
        if (pairs == null && pitchAccent == null) return emptyIterator; 
        else if (pairs == null) { 
            List<Pair<String,String>> retval = new ArrayList<Pair<String,String>>(1); 
            retval.add(new Pair<String,String>(Tokenizer.PITCH_ACCENT_ATTR, pitchAccent));
            return retval.iterator();
        }
        else if (pitchAccent == null) return pairs.iterator();
        else {
            List<Pair<String,String>> retval = new ArrayList<Pair<String,String>>(pairs);
            retval.add(new Pair<String,String>(Tokenizer.PITCH_ACCENT_ATTR, pitchAccent));
            return retval.iterator();
        }
    }            
    
    /** Returns the list of extra attribute-value pairs. */
    abstract protected List<Pair<String,String>> getAttrValPairsList();
    
    /** Returns the stem. */
    abstract public String getStem();
    
    /** Returns the part of speech. */
    abstract public String getPOS();
    
    /** Returns the supertag. */
    abstract public String getSupertag();
    
    /** Returns the semantic class. */
    abstract public String getSemClass();

    
    /** Returns the value of the attribute with the given name, or null if none. 
        The attribute names Tokenizer.WORD_ATTR, ..., Tokenizer.SEM_CLASS_ATTR 
        may be used to retrieve the form, ..., semantic class. */
    abstract public String getVal(String attr);

    
    // the known attr names
    private Set<String> knownAttrs = initKnownAttrs(); 
	private Set<String> initKnownAttrs() {
        Set<String> knownAttrs = new LinkedHashSet<String>();
        String[] names = {
            Tokenizer.WORD_ATTR, Tokenizer.PITCH_ACCENT_ATTR, 
            Tokenizer.STEM_ATTR, Tokenizer.POS_ATTR, 
            Tokenizer.SUPERTAG_ATTR, Tokenizer.SEM_CLASS_ATTR
        };
        for (int i = 0; i < names.length; i++) { knownAttrs.add(names[i]); }
        return knownAttrs;
    }
    
    /** Returns whether the given attr is a known one (vs an extra one). */
    public boolean isKnownAttr(String attr) {
        return knownAttrs.contains(attr.intern());
    }
    
    
    /** Returns true if the form is non-null, while the stem, part of speech, supertag and semantic class are null. */
    public boolean isSurfaceWord() {
        return getForm() != null && getStem() == null && getPOS() == null && getSupertag() == null && getSemClass() == null;
    }
    
    
    /** Creates a surface word with the given form. 
     * @param wordFactory TODO*/
    public static Word createWord(IWordFactory wordFactory, String form) { 
        form = (form != null) ? form.intern() : null; 
        return wordFactory.create(form);
    }
    
    /** Creates a (surface or full) word. 
     * @param wf TODO*/
    public static Word createWord(
        IWordFactory wf, String form, String pitchAccent, 
        List<Pair<String,String>> attrValPairs, String stem, String POS, String supertag, String semClass 
    ) {
        // normalize factors
        form = (form != null) ? form.intern() : null; 
        pitchAccent = (pitchAccent != null) ? pitchAccent.intern() : null;
        if (attrValPairs != null) {
            if (attrValPairs.isEmpty()) attrValPairs = null;
            else {
                attrValPairs = new ArrayList<Pair<String,String>>(attrValPairs);
                sortAttrValPairs(attrValPairs);
                for (int i = 0; i < attrValPairs.size(); i++) {
                    Pair<String,String> p = attrValPairs.get(i);
                    String attr = p.a.intern();
                    String val = (p.b != null) ? p.b.intern() : null;
                    attrValPairs.set(i, new Pair<String,String>(attr, val));
                }
            }
        }
        stem = (stem != null) ? stem.intern() : null; 
        POS = (POS != null) ? POS.intern() : null;
        supertag = (supertag != null) ? supertag.intern() : null;
        semClass = (semClass != null) ? semClass.intern() : null; 
        // create word
        return wf.create(form, pitchAccent, attrValPairs, stem, POS, supertag, semClass);
    }

    // comparator for attr-val pairs
    private static Comparator<Pair<String,String>> attrValComparator = new Comparator<Pair<String,String>>() {
        public int compare(Pair<String,String> p1, Pair<String,String> p2) {
            return p1.a.compareTo(p2.a);
        }
    };
    
    /** Sorts attr-val pairs by attr name. */
    private static void sortAttrValPairs(List<Pair<String,String>> pairs) {
        Collections.sort(pairs, attrValComparator);
    }
    
    /** Creates a (surface or full) word with the given attribute name and value.
        The attribute names Tokenizer.WORD_ATTR, ..., Tokenizer.SEM_CLASS_ATTR 
        may be used for the form, ..., semantic class. 
     * @param wordFactory TODO*/
    public static Word createWord(IWordFactory wordFactory, String attr, String val) {
        attr = attr.intern(); val = (val != null) ? val.intern() : null; 
        return wordFactory.create(attr, val);
    }
    
    /** Creates a (surface or full) word from the given one, replacing the word form with the given one. 
     * @param wf TODO*/
    public static Word createWord(IWordFactory wf, Word word, String form) {
        if (form != null) form = form.intern();
        return wf.create(form, word.getPitchAccent(), word.getAttrValPairsList(), word.getStem(), word.getPOS(), word.getSupertag(), word.getSemClass());
    }
    
    /** Creates a (surface or full) word from the given one, 
        replacing the form and stem with the semantic class, uppercased. 
     * @param wf TODO*/
    public static Word createWordUsingSemClass(IWordFactory wf, Word word) {
        String form = word.getSemClass().toUpperCase().intern();
        String stem = form;
        return wf.create(form, word.getPitchAccent(), word.getAttrValPairsList(), stem, word.getPOS(), word.getSupertag(), word.getSemClass());
    }
    
    /** Creates a (surface or full) word from the given surface one, adding the 
        second word's additional attr-val pairs. 
     * @param wf TODO*/
    public static Word createWordWithAttrs(IWordFactory wf, Word word, Word word2) {
        // get accent
        String accent = word.getPitchAccent();
        if (accent == null) accent = word2.getPitchAccent();
        // get attrs
        boolean mixedAttrs = false;
        List<Pair<String,String>> pairs = word.getAttrValPairsList();
        List<Pair<String,String>> pairs2 = word2.getAttrValPairsList(); 
        if (pairs == null && pairs2 != null) { pairs = pairs2; }
        else if (pairs2 != null) {
            mixedAttrs = true;
            pairs = new ArrayList<Pair<String,String>>(pairs); 
            for (int i = 0; i < pairs2.size(); i++) {
                if (!pairs.contains(pairs2.get(i))) {
                    pairs.add(pairs2.get(i)); 
                }
            }
        }
        // get rest
        String form = word.getForm(); String stem = word.getStem(); 
        String POS = word.getPOS(); String supertag = word.getSupertag(); String semClass = word.getSemClass(); 
        // with mixed attrs, need to normalize
        if (mixedAttrs) 
            return createWord(wf, form, accent, pairs, stem, POS, supertag, semClass);
        else 
            return wf.create(form, accent, pairs, stem, POS, supertag, semClass);
    }
    
    
    /** Creates a full word from the given surface one, adding the given stem, POS and semantic class. 
     * @param wf TODO*/
    public static Word createFullWord(IWordFactory wf, Word word, String stem, String POS, String supertag, String semClass) {
        stem = (stem != null) ? stem.intern() : null; 
        POS = (POS != null) ? POS.intern() : null;
        supertag = (supertag != null) ? supertag.intern() : null;
        semClass = (semClass != null) ? semClass.intern() : null; 
        return wf.create(word.getForm(), word.getPitchAccent(), word.getAttrValPairsList(), stem, POS, supertag, semClass);
    }
    
    /** Creates a full word from the given surface one, 
        adding the second (full) given word's stem, POS and semantic class, 
        as well as the second word's additional attr-val pairs, 
        plus the given supertag. 
     * @param wf TODO*/
    public static Word createFullWord(IWordFactory wf, Word word, Word word2, String supertag) {
        boolean mixedAttrs = false;
        List<Pair<String,String>> pairs = word.getAttrValPairsList(); 
        List<Pair<String,String>> pairs2 = word2.getAttrValPairsList(); 
        if (pairs == null && pairs2 != null) { pairs = pairs2; }
        else if (pairs2 != null) {
            mixedAttrs = true;
            pairs = new ArrayList<Pair<String,String>>(pairs); 
            for (int i = 0; i < pairs2.size(); i++) {
                if (!pairs.contains(pairs2.get(i))) {
                    pairs.add(pairs2.get(i)); 
                }
            }
        }
        if (mixedAttrs) { 
            return createWord(wf, word.getForm(), word.getPitchAccent(), pairs, word2.getStem(), word2.getPOS(), supertag, word2.getSemClass());
        }
        else {
            supertag = (supertag != null) ? supertag.intern() : null;
            return wf.create(word.getForm(), word.getPitchAccent(), pairs, word2.getStem(), word2.getPOS(), supertag, word2.getSemClass());
        }
    }
    

    /** Creates a surface word from the given one, removing the stem, POS, supertag and semantic class. 
     * @param wf TODO*/
    public static Word createSurfaceWord(IWordFactory wf, Word word) {
        return wf.create(word.getForm(), word.getPitchAccent(), word.getAttrValPairsList(), null, null, null, null);
    }
    
    /** Creates a surface word from the given one, removing the stem, POS, supertag and semantic class, 
        and replacing the form with the given one. 
     * @param wf TODO*/
    public static Word createSurfaceWord(IWordFactory wf, Word word, String form) {
        form = (form != null) ? form.intern() : null; 
        return wf.create(form, word.getPitchAccent(), word.getAttrValPairsList(), null, null, null, null);
    }
    
    /** Creates a surface word from the given one, removing the stem, POS, supertag and semantic class, 
        and replacing the form with the semantic class, uppercased. 
     * @param wf TODO*/
    public static Word createSurfaceWordUsingSemClass(IWordFactory wf, Word word) {
        String form = word.getSemClass().toUpperCase().intern();
        return wf.create(form, word.getPitchAccent(), word.getAttrValPairsList(), null, null, null, null);
    }

    
    /** Creates a core surface word from the given one, removing all attrs in the given set. 
     * @param wf TODO*/
    public static Word createCoreSurfaceWord(IWordFactory wf, Word word, CollectionContains<String> attrsSet) {
        String form = word.getForm();
        String accent = word.getPitchAccent();
        if (accent != null && attrsSet.contains(Tokenizer.PITCH_ACCENT_ATTR)) accent = null;
        List<Pair<String,String>> pairs = word.getAttrValPairsList(); 
        if (pairs != null) {
            pairs = new ArrayList<Pair<String,String>>(pairs); 
            Iterator<Pair<String,String>> pairsIt = pairs.iterator(); 
            while (pairsIt.hasNext()) {
                Pair<String,String> pair = pairsIt.next(); 
                if (attrsSet.contains(pair.a)) { pairsIt.remove(); }
            }
            return createWord(wf, form, accent, pairs, null, null, null, null);
        }
        else {
            return wf.create(form, accent, null, null, null, null, null);
        }
    }
    

    /** Returns a hash code for this word. */
    public int hashCode() {
        int hc = System.identityHashCode(getForm());
        hc = 31*hc + System.identityHashCode(getPitchAccent());
        for (Iterator<Pair<String,String>> it = getAttrValPairs(); it.hasNext(); ) {
            Pair<String,String> p = it.next();
            hc = 31*hc + System.identityHashCode(p.a);
            hc = 31*hc + System.identityHashCode(p.b);
        }
        hc = 31*hc + System.identityHashCode(getStem()); 
        hc = 31*hc + System.identityHashCode(getPOS()); 
        hc = 31*hc + System.identityHashCode(getSupertag()); 
        hc = 31*hc + System.identityHashCode(getSemClass()); 
        return hc;
    }
    
    /** Returns whether this word equals the given object. */
    public boolean equals(Object obj) {
        if (this == obj) return true;
        // nb: can use ==, since constructor interns all factors
        if (!(obj instanceof Word)) return false;
        Word word = (Word) obj;
        boolean sameFields =
            getForm() == word.getForm() && 
            getPitchAccent() == word.getPitchAccent() &&
            getStem() == word.getStem() && 
            getPOS() == word.getPOS() && 
            getSupertag() == word.getSupertag() && 
            getSemClass() == word.getSemClass();
        if (!sameFields) return false;
        List<Pair<String,String>> pairs = getAttrValPairsList();
        List<Pair<String,String>> wordPairs = word.getAttrValPairsList();
        if (pairs == null && wordPairs == null) return true;
        if (pairs == null || wordPairs == null) return false;
        if (pairs.size() != wordPairs.size()) return false;
        for (int i = 0; i < pairs.size(); i++) {
            if (!pairs.get(i).equals(wordPairs.get(i))) return false;
        }
        return true;
    }
    
    /** Returns an int representing lexicographic sorting. */
    public int compareTo(Word word) {
    	if (this == word) return 0;
    	int cmp = 0;
    	cmp = compare(getForm(), word.getForm());
    	if (cmp != 0) return cmp;
    	cmp = compare(getPitchAccent(), word.getPitchAccent());
    	if (cmp != 0) return cmp;
    	cmp = compare(getStem(), word.getStem());
    	if (cmp != 0) return cmp;
    	cmp = compare(getPOS(), word.getPOS());
    	if (cmp != 0) return cmp;
    	cmp = compare(getSupertag(), word.getSupertag());
    	if (cmp != 0) return cmp;
    	cmp = compare(getSemClass(), word.getSemClass());
    	if (cmp != 0) return cmp;
        List<Pair<String,String>> pairs = getAttrValPairsList();
        List<Pair<String,String>> wordPairs = word.getAttrValPairsList();
        if (pairs == null && wordPairs == null) return 0;
        if (pairs == null) return -1;
        if (wordPairs == null) return 1;
        if (pairs.size() < wordPairs.size()) return -1;
        if (pairs.size() > wordPairs.size()) return 1;
        for (int i = 0; i < pairs.size(); i++) {
        	Pair<String,String> p = pairs.get(i);
        	Pair<String,String> wp = wordPairs.get(i);
        	cmp = p.a.compareTo(wp.a);
        	if (cmp != 0) return cmp;
        	cmp = p.b.compareTo(wp.b);
        	if (cmp != 0) return cmp;
        }
    	return 0;
    }
    
    // compares strings, accounting for nulls
    private int compare(String s1, String s2) {
    	if (s1 == null && s2 == null) return 0;
    	if (s1 == null) return -1;
    	if (s2 == null) return 1;
    	return s1.compareTo(s2);
    }
    
    /** Returns whether this word's surface attributes intersect with the given ones. */
    public boolean attrsIntersect(CollectionContains<String> attrsSet) {
        if (getPitchAccent() != null && attrsSet.contains(Tokenizer.PITCH_ACCENT_ATTR))
            return true;
        for (Iterator<Pair<String,String>> it = getAttrValPairs(); it.hasNext(); ) {
            Pair<String,String> p = it.next();
            if (attrsSet.contains(p.a)) return true;
        }
        return false;
    }
    
    /** Returns a hash code for this word's restriction to a surface word. */
    public int surfaceWordHashCode() {
        int hc = System.identityHashCode(getForm());
        hc = 31*hc + System.identityHashCode(getPitchAccent());
        for (Iterator<Pair<String,String>> it = getAttrValPairs(); it.hasNext(); ) {
            Pair<String,String> p = it.next();
            hc = 31*hc + System.identityHashCode(p.a);
            hc = 31*hc + System.identityHashCode(p.b);
        }
        return hc;
    }
    
    /** Returns whether this word and the given object have equal restrictions to surface words. */
    public boolean surfaceWordEquals(Object obj) {
        if (this == obj) return true;
        // nb: can use ==, since constructor interns all factors
        if (!(obj instanceof Word)) return false;
        Word word = (Word) obj;
        boolean sameFields =
            getForm() == word.getForm() && 
            getPitchAccent() == word.getPitchAccent();
        if (!sameFields) return false;
        List<Pair<String,String>> pairs = getAttrValPairsList();
        List<Pair<String,String>> wordPairs = word.getAttrValPairsList();
        if (pairs == null && wordPairs == null) return true;
        if (pairs == null || wordPairs == null) return false;
        if (pairs.size() != wordPairs.size()) return false;
        for (int i = 0; i < pairs.size(); i++) {
            if (!pairs.get(i).equals(wordPairs.get(i))) return false;
        }
        return true;
    }
    
    /** Returns canonical version of deserialized word. 
     * @param wf TODO*/
    public Object readResolve(IWordFactory wf) throws ObjectStreamException {
    	return createWord(wf, getForm(), getPitchAccent(), getAttrValPairsList(), getStem(), getPOS(), getSupertag(), getSemClass());
    }
    
    /** Shows non-trivial fields separated by underscores. */
    public String toString() {
        StringBuffer sb = new StringBuffer();
        if (getForm() != null) sb.append(getForm());
        if (getPitchAccent() != null) sb.append('_').append(getPitchAccent());
        for (Iterator<Pair<String,String>> it = getAttrValPairs(); it.hasNext(); ) {
            Pair<String,String> p = it.next();
            sb.append('_').append(p.b);
        }
        if (getStem() != null && getStem() != getForm()) sb.append('_').append(getStem());
        if (getPOS() != null) sb.append('_').append(getPOS());
        if (getSupertag() != null) sb.append('_').append(getSupertag());
        if (getSemClass() != null) sb.append('_').append(getSemClass());
        if (sb.length() == 0) sb.append((String)null);
        return sb.toString();
    }
}

