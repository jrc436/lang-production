///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2005 University of Edinburgh (Michael White)
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

import util.*;

import java.util.*;

/**
 * A FactorChainWord represents a word via a chain of references to 
 * factor keys, or just to a string in the case of the word form. 
 * Canonical instances are created by a factory method, and stored 
 * in a trie map.  The factor chain representation should be more 
 * space efficient when dealing with large numbers of words.
 *
 *
 * @author      Michael White
 * @version     $Revision: 1.3 $, $Date: 2009/07/17 04:23:30 $
 */
public class FactorChainWord extends Word {
    
	private static final long serialVersionUID = 952665894357382685L;

	/** The referenced factor key or string (for the word form). */
    protected Object key;
    
    /** The previous node in the chain. */
    protected FactorChainWord prev;
    
    /** Constructor. */
    protected FactorChainWord(Object key, FactorChainWord prev) {
        this.key = key; this.prev = prev; 
    }
    
    
    /** Returns the surface form. */
    public String getForm() { return getValFromInterned(Tokenizer.WORD_ATTR); }
    
    /** Returns the pitch accent. */
    public String getPitchAccent() { return getValFromInterned(Tokenizer.PITCH_ACCENT_ATTR); }
    
    /** Returns the list of extra attribute-value pairs. */
    protected List<Pair<String,String>> getAttrValPairsList() { 
        List<Pair<String,String>> retval = null; 
        FactorChainWord current = this;
        while (current != null) {
            if (current.key instanceof FactorKey) {
                FactorKey fkey = (FactorKey) current.key;
                if (!isKnownAttr(fkey.factor)) {
                    if (retval == null) retval = new ArrayList<Pair<String,String>>(5);
                    retval.add(0, new Pair<String,String>(fkey.factor, fkey.val));
                }
            }
            current = current.prev;
        }
        return retval; 
    }
    
    /** Returns the stem. */
    public String getStem() { return getValFromInterned(Tokenizer.STEM_ATTR); }
    
    /** Returns the part of speech. */
    public String getPOS() { return getValFromInterned(Tokenizer.POS_ATTR); }
    
    /** Returns the supertag. */
    public String getSupertag() { return getValFromInterned(Tokenizer.SUPERTAG_ATTR); }
    
    /** Returns the semantic class. */
    public String getSemClass() { return getValFromInterned(Tokenizer.SEM_CLASS_ATTR); }

    
    /** Returns the value of the attribute with the given name, or null if none. 
        The attribute names Tokenizer.WORD_ATTR, ..., Tokenizer.SEM_CLASS_ATTR 
        may be used to retrieve the form, ..., semantic class. */
    public String getVal(String attr) {
        String internedAttr = attr.intern(); // use == on interned attr
        return getValFromInterned(internedAttr); 
    }
    
    /** Returns the value of the given interned attr, or null if none. */
    protected String getValFromInterned(String attr) {
        FactorChainWord current = this;
        while (current != null) {
            if (attr == Tokenizer.WORD_ATTR) {
                if (current.key instanceof String) return (String) current.key;
            }
            else if (current.key instanceof FactorKey) {
                FactorKey fkey = (FactorKey) current.key;
                if (fkey.factor == attr) return fkey.val;
            }
            current = current.prev;
        }
        return null;
    }   
}

