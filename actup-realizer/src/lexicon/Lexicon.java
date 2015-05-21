///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2004-9 Jason Baldridge, Gann Bierner and 
//                      Michael White (University of Edinburgh, The Ohio State University)
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

import grammar.TypesData;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jdom.Element;

import synsem.AtomCat;
import synsem.Category;
import synsem.LF;
import unify.FeatureStructure;
import util.CollectionContains;
import util.GroupMap;
import util.Pair;
import util.XmlScanner;


/**
 * A Lexicon is the IMMUTABLE, THREAD-SAFE read of the openCCG lexicon files. 
 * 
 *
 * @author      Gann Bierner
 * @author      Jason Baldridge
 * @author      Michael White
 * @author		Jeremy Cole
 * @version     $Revision: 2.0 $, $Date: 2015/05/05 $
 */
public class Lexicon { 
	 // default relation sort order
    private final static String[] defaultRelationSortOrder = {
        "BoundVar", "PairedWith", "Restr", "Body", "Scope", "*", "GenRel", "Coord", "Append"
    };
    /** Flag used to indicate a purely syntactic edge, with no associated semantics. */
    public static final String NO_SEM_FLAG = "*NoSem*";
    
    /** Constant used to signal the substitution of the stem or pred. */
    public static final String DEFAULT_VAL = "[*DEFAULT*]";
    
    // various maps
    private GroupMap<Word, MorphItem> _words;
    public Set<MorphItem> getMorphItems(Word w) {
    	return _words.get(w);
    }
    
    private GroupMap<String, StemStruct> _stems;
    public boolean stemsContains(String o) {
    	return _stems.containsKey(o);
    }
    public Set<StemStruct> stemsGet(String s) {
    	return _stems.get(s);
    }
    
    private GroupMap<String,FeatureStructure> _macros;
    public Set<FeatureStructure> getMacros(String name) {
    	return _macros.get(name);
    }
    private Map<String,MacroItem> _macroItems;
    public MacroItem getMacroItem(String name) {
    	return _macroItems.get(name);
    }

    private GroupMap<String, EntriesItem[]> _posToEntries;
    public Set<EntriesItem[]> posToEntriesGet(String s) {
    	return _posToEntries.get(s);
    }
    
    private GroupMap<String, EntriesItem> _stagToEntries;
    private GroupMap<String, Word> _predToWords;
    public Set<Word> getWordsFromPred(String pred) {
    	return _predToWords.get(pred);
    }
    
    private GroupMap<String, String> _relsToPreds;
    public Set<String> relsToPredsGet(String s) {
    	return _relsToPreds.get(s);
    }
    
    private GroupMap<String, String> _coartRelsToPreds;
    public Set<String> coartRelsToPredsGet(String s) {
    	return _coartRelsToPreds.get(s);
    }
    
    private GroupMap<String, String> _catsToAttrs;
    public Set<String> getCatsToAttrs(String s) {
    	return _catsToAttrs.get(s);
    }
    
    private Set<String> _lfAttrs;
    public boolean lfattrsContains(String s) {
    	return _lfAttrs.contains(s);
    }
    
    // coarticulation attrs
    private Set<String> _coartAttrs;
    public CollectionContains<String> coartAttrsContains() {
    	return _coartAttrs::contains;
    }
    
    private Set<String> _indexedCoartAttrs;
    public boolean indexedCoartAttrsContains(String s) {
    	return _indexedCoartAttrs.contains(s);
    }
    
    private String[] _distributiveAttrs;
    public int numDistrAttrs() {
    	return _distributiveAttrs.length;
    }
    public String getDistrAttr(int index) {
    	return _distributiveAttrs[index];
    }
    
    //licensing features are immutable!
    private LicensingFeature[] _licensingFeatures;
    public LicensingFeature[] copyFeatures() {
    	return Arrays.copyOf(_licensingFeatures, _licensingFeatures.length);
    }
//    public LicensingFeature getLicensingFeature(int index) {
//    	return _licensingFeatures[index];  
//    }
    
    private Map<String,Integer> _relationIndexMap;

    
    private final TypesData td;
    private final Tokenizer t;
    private final IWordFactory wf;
    
    /** The grammar that this lexicon is part of. */
   // private final Grammar grammar;   
    
    /** Loads the lexicon and morph files. */
    public Lexicon(TypesData td, Tokenizer t, IWordFactory wf)  {
    	this.td = td;
    	this.t = t;
    	this.wf = wf;

       
        _catsToAttrs = new GroupMap<String,String>();
        _lfAttrs = new LinkedHashSet<String>();
    }
    public void init(boolean openlex, URL lexiconUrl, URL morphUrl) throws IOException {
        
        //HAVE TO LOOK AT LEXICON CREATION VS. INITIALIZING
         List<MorphItem> morph;
         List<MacroItem> macroModel;
        LexiconScanner ls = new LexiconScanner(lexiconUrl);
        _relationIndexMap = ls.loadRelationSortOrder(defaultRelationSortOrder);
        _distributiveAttrs = ls.getDistrAttrs();
        _licensingFeatures = ls.loadLicensingFeatures();
        List<Family> lexicon = ls.loadFamilies(this, this.td);
        
        
        Pair<List<MorphItem>,List<MacroItem>> morphInfo = getMorph(morphUrl);
        morph = morphInfo.a; macroModel = morphInfo.b;

        // index words; also index stems to words, as default preds
        // store indexed coarticulation attrs too
        _words = new GroupMap<Word,MorphItem>();
        _predToWords = new GroupMap<String,Word>();
        _coartAttrs = new LinkedHashSet<String>();
        _indexedCoartAttrs = new LinkedHashSet<String>();
        for (MorphItem morphItem : morph) {
            Word surfaceWord = morphItem.getSurfaceWord();
            _words.put(surfaceWord, morphItem);
            _predToWords.put(morphItem.getWord().getStem(), surfaceWord);
            if (morphItem.isCoart()) {
                Word indexingWord = morphItem.getCoartIndexingWord();
                _words.put(indexingWord, morphItem);
                Pair<String,String> first = indexingWord.getSurfaceAttrValPairs().next();
                _indexedCoartAttrs.add(first.a);
                for (Iterator<Pair<String,String>> it = surfaceWord.getSurfaceAttrValPairs(); it.hasNext(); ) {
                	Pair<String,String>  p = it.next();
                    _coartAttrs.add(p.a);
                }
            }
        }

        // index entries based on stem+pos
        _stems = new GroupMap<String, StemStruct>();
        _posToEntries = new GroupMap<String,EntriesItem[]>();
        // index entries by supertag+pos, for supertagging
        _stagToEntries = new GroupMap<String,EntriesItem>();
        // also index rels and coart rels to preds
        _relsToPreds = new GroupMap<String,String>();
        _coartRelsToPreds = new GroupMap<String,String>();
        // and gather list of attributes used per atomic category type 
          
        
        // index each family
        HashSet<String> familyAndEntryNames = new HashSet<String>();
        for (Family family : lexicon) {
        	indexFamily(family, openlex, familyAndEntryNames);
        }
        
        // index the macros
        _macros = new GroupMap<String, FeatureStructure>();
        // nb: could just index MacroItem objects for feature structures too;
        //     this might be a bit cleaner, but life is short
        _macroItems = new HashMap<String, MacroItem>();
        for (MacroItem mi : macroModel) {
            String macName = mi.getName();
            FeatureStructure[] specs = mi.getFeatureStructures();
            for (int j=0; j < specs.length; j++) {
                _macros.put(macName, specs[j]);
            }
            // this is for handling LF part of macros
            _macroItems.put(macName, mi);
        }

        // with morph items, check POS, macro names, excluded list for xref
        for (MorphItem morphItem : morph) {
            Word w = morphItem.getWord();
            if (!openlex && 
            	!_stems.containsKey(w.getStem() + w.getPOS()) &&
                !_posToEntries.containsKey(w.getPOS())) 
            {
                System.err.println(
                    "Warning: no entries for stem '" + w.getStem() + 
                    "' and POS '" + w.getPOS() + 
                    "' found for word '" + w + "'"
                );
            }
            String[] macroNames = morphItem.getMacros();
            for (int j=0; j < macroNames.length; j++) {
                if (!_macroItems.containsKey(macroNames[j])) {
                    System.err.println("Warning: macro " + macroNames[j] + 
                        " not found for word '" + morphItem.getWord() + "'");
                }
            }
            String[] excludedNames = morphItem.getExcluded();
            for (int j=0; j < excludedNames.length; j++) {
                if (!familyAndEntryNames.contains(excludedNames[j])) {
                    System.err.println("Warning: excluded family or entry '" + excludedNames[j] + 
                        "' not found for word '" + morphItem.getWord() + "'");
                }
            }
        }	
    }
    //modifies the familyAndEntryNames set
    private void indexFamily(Family family, boolean openlex, Set<String> familyAndEntryNames) {
    	// and remember family and ent, names, for checking excluded list on morph items
        //HashSet<String> familyAndEntryNames = new HashSet<String>();
    	familyAndEntryNames.add(family.getName());
        EntriesItem[] entries = family.getEntries();
        DataItem[] data = family.getData();

        // for generic use when we get an unknown stem
        // from the morphological analyzer
        if (!family.isClosed()) {
            _posToEntries.put(family.getPOS(), entries);
        }

        // scan through entries
        for (int j=0; j < entries.length; j++) {
            // index
            EntriesItem eItem = entries[j];
        	_stagToEntries.put(eItem.getSupertag()+family.getPOS(), eItem);
            if (eItem.getStem().length() > 0) {
                _stems.put(eItem.getStem()+family.getPOS(), new StemStruct(eItem));
            }
            try {
                // gather features
                eItem.getCat().applyToAll(this::gatherAttrs);
                // record names
                familyAndEntryNames.add(eItem.getName());
                familyAndEntryNames.add(eItem.getQualifiedName());
            }
            catch (RuntimeException exc) {
                System.err.println("exception for: " + family.getName() + ": " + exc);
            }
        }

        // scan through data
        for (int j=0; j < data.length; j++) {
            DataItem dItem = data[j];
            _stems.put(dItem.getStem()+family.getPOS(), new StemStruct(entries, dItem));
            // index non-default preds to words
            if (!dItem.getStem().equals(dItem.getPred())) {
                Collection<Word> words = (Collection<Word>) _predToWords.get(dItem.getStem());
                if (words == null) {
                	if (!openlex) {
                        System.err.print("Warning: couldn't find words for pred '");
                        System.err.println(dItem.getPred() + "' with stem '" + dItem.getStem() + "'");
                	}
                }
                else {
                    for (Iterator<Word> it = words.iterator(); it.hasNext(); ) {
                        _predToWords.put(dItem.getPred(), it.next());
                    }
                }
            }
        }

        // index rels to preds
        // nb: this covers relational (eg @x<GenRel>e) and featural (eg @e<tense>past) 
        //     elementary predications
        List<String> indexRels = new ArrayList<String>(3);
        String familyIndexRel = family.getIndexRel();
        if (familyIndexRel.length() > 0) { 
            indexRels.add(familyIndexRel); 
        }
        for (int j=0; j < entries.length; j++) {
            EntriesItem eItem = entries[j];
            String indexRel = eItem.getIndexRel();
            if (indexRel.length() > 0 && !indexRel.equals(familyIndexRel)) {
                indexRels.add(indexRel);
            }
        }
        for (Iterator<String> it = indexRels.iterator(); it.hasNext(); ) {
            String indexRel = it.next();
            // nb: not indexing on entries items, b/c some stems are still defaults 
            for (int j=0; j < data.length; j++) {
                DataItem dItem = data[j];
                _relsToPreds.put(indexRel, dItem.getPred());
            }
        }
        
        // index coart rels (features, really) to preds
        String coartRel = family.getCoartRel();
        if (coartRel.length() > 0) {
            for (int j=0; j < data.length; j++) {
                _coartRelsToPreds.put(coartRel, data[j].getPred());
            }
        }
    }

   
    
    
 // gathers attrs from a category
    private void gatherAttrs(Category c) {
        if (!(c instanceof AtomCat)) return;
        String type = ((AtomCat)c).getType();
        FeatureStructure fs = c.getFeatureStructure();
        if (fs == null) return;
        for (Iterator<String> it = fs.getAttributes().iterator(); it.hasNext(); ) {
            String att = it.next();
            _catsToAttrs.put(type, att);
            if (fs.getValue(att) instanceof LF) {
                _lfAttrs.add(att);
            }
        }
    }
    
    // get licensing features, with appropriate defaults
	
    /**
     * Returns the index of the given relation in the relation sort order, 
     * or the index of "*" if the relation is not explicitly listed.
     */
    public Integer getRelationSortIndex(String rel) {
        Integer retval = _relationIndexMap.get(rel);
        if (retval != null) return retval;
        retval = _relationIndexMap.get("*");
        if (retval != null) return retval;
        return new Integer(-1);
    }
    
    
    //
    // access to maps (limited)
    //

    /** Returns whether the given rel (semantic feature, really) is one used to signal coarticulation. */
    public boolean isCoartRel(String rel) {
        return _coartRelsToPreds.containsKey(rel);
    }
    
    
	private class MorphScanner extends XmlScanner {
    	List<MorphItem> morphItems = new ArrayList<MorphItem>();
    	List<MacroItem> macroItems = new ArrayList<MacroItem>();
    	public void handleElement(Element e) {
            // create morph item
			if (e.getName().equals("entry")) {
                try { morphItems.add(new MorphItem(t, wf, e)); }
                catch (RuntimeException exc) {
                    System.err.println("Skipping morph item: " + e.getAttributeValue("word"));
                    exc.printStackTrace();
                }
            }
            // create macro item
			else if (e.getName().equals("macro")) {
                try { macroItems.add(new MacroItem(Lexicon.this, td, e)); }
                catch (RuntimeException exc) {
                    System.err.println("Skipping macro item: " + e.getAttributeValue("name"));
                    System.err.println(exc.toString());
                }
            }
		}
	};
	
    private Pair<List<MorphItem>,List<MacroItem>> getMorph(URL url) throws IOException {
    	// scan XML
    	MorphScanner morphScanner = new MorphScanner();
    	morphScanner.parse(url);
        return new Pair<List<MorphItem>,List<MacroItem>>(morphScanner.morphItems, morphScanner.macroItems);
    }
}
class LexiconScanner {
	private class LexScan extends XmlScanner {
		
		public void handleElement(Element e) {
	        // save distributive attributes
			if (e.getName().equals("distributive-features")) distrElt = e; 
	        // save licensing features
			else if (e.getName().equals("licensing-features")) licensingElt = e; 
	        // save relation sort order
			else if (e.getName().equals("relation-sorting")) relationSortingElt = e; 
			else if (e.getName().equals("family")) {
				lexicon.add(e);
            }
		}
	}
	private Element distrElt = null;
	private Element licensingElt = null;
	private Element relationSortingElt = null;
	private List<Element> lexicon = new ArrayList<Element>();
	public LexiconScanner(URL url) throws IOException {
		LexScan ls = new LexScan();
		ls.parse(url);
	}
	public List<Family> loadFamilies(Lexicon l, TypesData td) {
		List<Family> lex = new ArrayList<Family>();
		for (Element e : lexicon) {
			try {
				lex.add(new Family(l, td, e));
			}
	        catch (RuntimeException exc) {
	            System.err.println("Skipping family: " + e.getAttributeValue("name"));
	            exc.printStackTrace();
	        }			
		}
		return lex;
	}
	
	//These methods must be called AFTER parse to work correctly.
	public Map<String, Integer> loadRelationSortOrder(String[] defaultRelationSortOrder) {
		// use defaults if no order specified
    	Map<String, Integer> relIndexMap = new LinkedHashMap<String, Integer>();
        if (relationSortingElt == null) {
            for (int i = 0; i < defaultRelationSortOrder.length; i++) {
            	relIndexMap.put(defaultRelationSortOrder[i], new Integer(i));
            }
            return relIndexMap;
        }
        // otherwise load from 'order' attribute
        String orderAttr = relationSortingElt.getAttributeValue("order");
        String[] relSortOrder = orderAttr.split("\\s+");
        for (int i = 0; i < relSortOrder.length; i++) {
        	relIndexMap.put(relSortOrder[i], new Integer(i));
        }
        return relIndexMap;
	}
	public String[] getDistrAttrs() {
		return distrElt != null ? distrElt.getAttributeValue("attrs").split("\\s+") : new String[0];
	}
	public LicensingFeature[] loadLicensingFeatures() {
        List<LicensingFeature> licensingFeats = new ArrayList<LicensingFeature>();
        boolean containsLexFeat = false;
        if (licensingElt != null) {
            for (@SuppressWarnings("unchecked")
			Iterator<Element> it = licensingElt.getChildren("feat").iterator(); it.hasNext(); ) {
                Element featElt = it.next();
                String attr = featElt.getAttributeValue("attr");
                if (attr.equals("lex")) containsLexFeat = true;
                String val = featElt.getAttributeValue("val");
                List<String> alsoLicensedBy = null;
                String alsoVals = featElt.getAttributeValue("also-licensed-by");
                if (alsoVals != null) {
                    alsoLicensedBy = Arrays.asList(alsoVals.split("\\s+"));
                }
                boolean licenseEmptyCats = true;
                boolean licenseMarkedCats = false;
                boolean instantiate = true; 
                byte loc = LicensingFeature.BOTH;
                String lmc = featElt.getAttributeValue("license-marked-cats");
                if (lmc != null) {
                    licenseMarkedCats = Boolean.valueOf(lmc).booleanValue();
                    // change defaults
                    licenseEmptyCats = false;
                    loc = LicensingFeature.TARGET_ONLY;
                    instantiate = false;
                }
                String lec = featElt.getAttributeValue("license-empty-cats");
                if (lec != null) {
                    licenseEmptyCats = Boolean.valueOf(lec).booleanValue();
                }
                String inst = featElt.getAttributeValue("instantiate");
                if (inst != null) {
                    instantiate = Boolean.valueOf(inst).booleanValue();
                }
                String locStr = featElt.getAttributeValue("location");
                if (locStr != null) {
                    if (locStr.equals("target-only")) loc = LicensingFeature.TARGET_ONLY;
                    if (locStr.equals("args-only")) loc = LicensingFeature.ARGS_ONLY;
                    if (locStr.equals("both")) loc = LicensingFeature.BOTH;
                }
                licensingFeats.add(
                    new LicensingFeature(
                        attr, val, alsoLicensedBy, 
                        licenseEmptyCats, licenseMarkedCats, instantiate, 
                        loc
                    )
                );
            }
        }
        if (!containsLexFeat) {
            licensingFeats.add(LicensingFeature.defaultLexFeature);
        }
        //_licensingFeatures = new LicensingFeature[licensingFeats.size()];
        return licensingFeats.toArray(new LicensingFeature[licensingFeats.size()]);
    }
	
};
