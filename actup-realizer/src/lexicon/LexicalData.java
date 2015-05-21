package lexicon;

import grammar.RuleGroupData;
import grammar.TypesData;
import hylo.HyloVar;
import hylo.NominalVar;
import hylo.Proposition;
import hylo.SatOp;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import synsem.AtomCat;
import synsem.Category;
import synsem.Sign;
import synsem.SignSurfaceWords;
import unify.FeatureStructure;
import unify.GFeatStruc;
import unify.GFeatVar;
import unify.Mutable;
import unify.SimpleType;
import unify.UnifyControl;
import unify.UnifyFailure;
import util.GroupMap;
import util.Pair;

/**
 * A lexical data is closely coupled with a Lexicon. The main difference is that it is NOT thread-safe and contains Mutable structures
 * @author Jeremy Cole
 */

public class LexicalData {
    private final Lexicon lexicon;
    private final UnifyControl uc;
    private RuleGroupData rgd = null;
    private final TypesData td;
    private final IWordFactory wf;
    private final Tokenizer t;
    public LexicalData(Lexicon lex, UnifyControl uc, TypesData td, IWordFactory wf, Tokenizer t) {
    	this.lexicon = lex;
    	this.uc = uc;
    	this.td = td;
    	this.wf = wf;
    	this.t = t;
    }
    //use with caution. Only used because of the cross-dependency;
    public void setRuleGroupData(RuleGroupData rgd) {
    	if (this.rgd != null) {
    		System.err.println("Do not reset the RuleGroupData. It should be set once at creation, and then never again.");
    		System.exit(1);
    	}
    	this.rgd = rgd;
    }
    // a map from indices to atomic categories, reset for each category
    private Map<Integer, FeatureStructure> featStrucMap = new LinkedHashMap<Integer, FeatureStructure>();
    // attrs per atomic category type, across all entries
   
    // a cache for macro adders
    private Map<MorphItem, MacroAdder> macAdderMap = new HashMap<MorphItem, MacroAdder>();
    // the replacement string for defaultReplacer
    private String REPLACEMENT = "";
    // the sem class for defaultNomvarSetter
    private SimpleType SEMCLASS = null;
    private Map<String,SimpleType> nomvarMap = new LinkedHashMap<String, SimpleType>();
 // an array of lists, one for each distributive attr    
 	private List<List<Object>> distrAttrVals = null;
 // given EntriesItem
 	
 	
    private void getWithEntriesItem(Word w, MorphItem mi, 
                                    String stem, String pred, 
                                    String targetPred, String targetRel,
                                    EntriesItem item,
                                    MacroAdder macAdder,
                                    Map<String,Double> supertags,
                                    Set<String> supertagsFound,
                                    Map<SignSurfaceWords, Sign> result) 
    {
        // ensure apropos
        if (targetPred != null && !targetPred.equals(pred)) return; 
        if (targetRel != null && !targetRel.equals(item.getIndexRel()) && !targetRel.equals(item.getCoartRel())) return; 
        if (!item.getActive().booleanValue()) return;
        if (mi.excluded(item)) return;
        
        try {
	        // copy and add macros
	        Category cat = item.getCat().copy();
	        macAdder.addMacros(cat);
	
	        // replace DEFAULT_VAL with pred, after first 
	        // unifying type of associated nom var(s) with sem class 
	        unifySemClass(cat, mi.getWord().getSemClass());
	        REPLACEMENT = pred; 
	        cat.mutateAll(this::defaultReplacer);
	        
	        // check supertag
	        // TODO: think about earlier checks for efficiency, for grammars where macros and preds don't matter
	        //Double lexprob = null; // nb: skipping lex log probs, don't seem to be helpful
	        if (supertags != null) {
	        	// skip if not found
	        	String stag = cat.getSupertag();
	        	if (!supertags.containsKey(stag)) return;
	        	// otherwise update found supertags
	        	supertagsFound.add(stag);
	        	// get lex prob
	        	//lexprob = supertags.get(stag);
	        }
	        
	        // propagate types of nom vars
	        propagateTypes(cat);
	        
	        // handle distrib attrs and inherits-from
	        propagateDistributiveAttrs(cat);
	        expandInheritsFrom(cat);
	        
	        // merge stem, pos, sem class from morph item, plus supertag from cat
	        Word word = Word.createFullWord(wf, w, mi.getWord(), cat.getSupertag());

	        // set origin and lexprob
	        Sign sign = new Sign(rgd, t, word, cat);
	        sign.setOrigin();
	        //if (lexprob != null) {
	        //	sign.addData(new SupertaggerAdapter.LexLogProb((float) Math.log10(lexprob)));
	        //}
	        // return sign
	        SignSurfaceWords sw = new SignSurfaceWords(sign);
	        if (result.get(sw) == null || sign.getDerivationHistory().compareTo(result.get(sw).getDerivationHistory()) > 0) {
	        	result.put(sw, sign);
	        }
        }
        catch (RuntimeException exc) {
        	System.err.println("Warning: ignoring entry: "+item.getName()+" of family: "+item.getFamilyName()+" for stem: "+stem + " b/c: " + exc.toString());
        	exc.printStackTrace();
        }
    }

 // given MorphItem
    private void getWithMorphItem(Word w, MorphItem mi, String targetPred, String targetRel, Map<SignSurfaceWords, Sign> result)
        throws LexException 
    {
    	// get supertags for filtering, if a supertagger is installed
    	Map<String,Double> supertags = null;
    	Set<String> supertagsFound = null;
    	
        // get macro adder
        MacroAdder macAdder = getMacAdder(mi);
        
        // if we have this stem in our lexicon
        String stem = mi.getWord().getStem();
        String pos = mi.getWord().getPOS();
        Set<EntriesItem[]> explicitEntries = null; // for storing entries from explicitly listed family members
        if (lexicon.stemsContains(stem+pos)) {
            explicitEntries = new HashSet<EntriesItem[]>();
            Set<StemStruct> stemItems = lexicon.stemsGet(stem+pos);
            for (Iterator<StemStruct> I = stemItems.iterator(); I.hasNext();) {
                StemStruct item = I.next();
                //in this case, there is a single entries item
                if (item.dItem == null) {
                    // do lookup
                    getWithEntriesItem(w, mi, stem, stem, targetPred, targetRel, item.eItems[0], macAdder, supertags, supertagsFound, result);
                } 
                // otherwise it has to have a DataItem and an EntriesItem[]
                else {
                    explicitEntries.add(item.eItems);
                    // do lookup
                    getWithDataItem(w, mi, item.dItem, item.eItems, targetPred, targetRel, macAdder, supertags, supertagsFound, result);
                }
            }
        }
        
        // for entries that are not explicitly in the lexicon file, we have to create
        // Signs from the open class entries with the appropriate part-of-speech
        Set<EntriesItem[]> entrySets = lexicon.posToEntriesGet(pos);
        if (entrySets != null) {
	        for (Iterator<EntriesItem[]> E=entrySets.iterator(); E.hasNext(); ) {
	            EntriesItem[] entries = E.next();  
	            // skip if entries explicitly listed
	            if (explicitEntries != null && explicitEntries.contains(entries)) continue;
	            // otherwise get entries with pred = targetPred, or stem if null
	            String pred = (targetPred != null) ? targetPred : stem;
	            getWithDataItem(w, mi, new DataItem(stem, pred), entries, targetPred, targetRel, macAdder, supertags, supertagsFound, result);
	        }
        }
      
    }
    // get signs with additional args for a known special token const, target pred and target rel        
    private Map<SignSurfaceWords, Sign> getSignsFromWord(Word w, String specialTokenConst, String targetPred, String targetRel) throws LexException {

        Set<MorphItem> morphItems = (specialTokenConst == null)
            ? lexicon.getMorphItems(w)
            : null;

        if (morphItems == null) {
            // check for special tokens
            if (specialTokenConst == null) {
                specialTokenConst = t.getSpecialTokenConstant(t.isSpecialToken(w.getForm()));
                targetPred = w.getForm();
            }
            if (specialTokenConst != null) {
                Word key = Word.createSurfaceWord(wf, w, specialTokenConst);
                morphItems = lexicon.getMorphItems(key);
            }
            // otherwise throw lex exception
            if (morphItems == null)
                throw new LexException(w + " not in lexicon");
        }

        Map<SignSurfaceWords, Sign> result = new LinkedHashMap<SignSurfaceWords, Sign>();

        for (Iterator<MorphItem> MI = morphItems.iterator(); MI.hasNext();) {
            getWithMorphItem(w, MI.next(), targetPred, targetRel, result);
        }

        return result;
    }
    /**
     * For a given word, return all of its surface word's lexical entries.
     * If the word is not listed in the lexicon, the tokenizer is 
     * consulted to see if it is a special token (date, time, etc.); 
     * otherwise an exception is thrown.
     * If the word has coarticulations, all applicable coarticulation 
     * entries are applied to the base word, in an arbitrary order.
     * @param w the word
     *
     * @return a sign hash
     * @exception LexException thrown if word not found
     */
    public Map<SignSurfaceWords, Sign> getSignsFromWord(Word w) throws LexException {
        // reduce word to its core, removing coart attrs if any
    	Word surfaceWord = Word.createSurfaceWord(wf, w);
        Word coreWord = (surfaceWord.attrsIntersect(lexicon.coartAttrsContains())) 
            ? Word.createCoreSurfaceWord(wf, surfaceWord, lexicon.coartAttrsContains()) 
            : surfaceWord;
        // lookup core word
            Map<SignSurfaceWords, Sign> result = getSignsFromWord(coreWord, null, null, null);
        if (result.size() == 0) {
            throw new LexException(coreWord + " not found in lexicon");
        }
        // return signs if no coart attrs
        if (coreWord == surfaceWord) return result; 
        // otherwise apply coarts for word
        applyCoarts(surfaceWord, result);
        return result; 
    }
    
    /**
     * Returns the lexical signs indexed by the given pred.
     * If the pred is not listed in the lexicon, the tokenizer is 
     * consulted to see if it is a special token (date, time, etc.); 
     * otherwise, null is returned.
     * Coarticulations are applied for the given rels, if non-null.
     */
    public Collection<Sign> getSignsFromPred(String pred, List<String> coartRels) { 	
        // lookup pred
        Collection<Sign> result = getSignsFromPredAndTargetRel(pred, null);
        if (result == null) {
        	return null;
        }
        // apply coarts for rels
        if (coartRels != null) {
        	applyCoarts(coartRels, result);
        }
        // and return
        return result;
    }
        
   
    
    // look up and apply coarts for given rels to each sign in result
    private void applyCoarts(List<String> coartRels, Collection<Sign> result) {
        List<Sign> inputSigns = new ArrayList<Sign>(result);
        result.clear();
        List<Sign> outputSigns = new ArrayList<Sign>(inputSigns.size());
        // for each rel, lookup coarts and apply to input signs, storing results in output signs
        for (Iterator<String> it = coartRels.iterator(); it.hasNext(); ) {
            String rel = it.next();
            Set<String> preds = lexicon.coartRelsToPredsGet(rel);
            if (preds == null) continue; // not expected
            Collection<Sign> coartResult = getSignsFromRelAndPreds(rel, preds);
            if (coartResult == null) continue;
            for (Iterator<Sign> it2 = coartResult.iterator(); it2.hasNext(); ) {
                Sign coartSign = it2.next();
                // apply to each input
                for (int j = 0; j < inputSigns.size(); j++) {
                    Sign sign = inputSigns.get(j);
                    rgd.applyCoart(sign, coartSign, outputSigns);
                }
            }
            // switch output to input for next iteration
            inputSigns.clear();
            inputSigns.addAll(outputSigns);
            outputSigns.clear();
        }
        // add results back
        result.addAll(inputSigns);
    }
    
    // given DataItem
    private void getWithDataItem(Word w, MorphItem mi,  
                                 DataItem item, EntriesItem[] entries, 
                                 String targetPred, String targetRel, 
                                 MacroAdder macAdder,
                                 Map<String,Double> supertags,
                                 Set<String> supertagsFound,
                                 Map<SignSurfaceWords, Sign> result) 
    {
        for (int i=0; i < entries.length; i++) {
            EntriesItem entry = entries[i];
            if (entry.getStem().equals(Lexicon.DEFAULT_VAL)) {
                getWithEntriesItem(w, mi, item.getStem(), item.getPred(), targetPred, targetRel, entry, macAdder, supertags, supertagsFound, result);
            }
        }
    }
    
    // unify sem class with default nom var(s)
    private void unifySemClass(Category cat, String semClass) {
        if (semClass == null || cat.getLF() == null) return;
        SEMCLASS = td.getSimpleType(semClass);
        try {
            cat.getLF().mutateAll(this::defaultNomvarUnifier);
        } catch (TypePropagationException tpe) {
            System.err.println(
                "Warning: unable to unify types '" + tpe.st1 + "' and '" + tpe.st2 + 
                "' in unifying sem class in cat: \n" + cat
            );
            tpe.printStackTrace();
        }
    }
    
    // mod function to unify type of nom var for DEFAULT_VAL with SEMCLASS
    private void defaultNomvarUnifier(Mutable m) {
            if (!(m instanceof SatOp)) return;
            SatOp satop = (SatOp) m;
            if (!(satop.getArg() instanceof Proposition)) return; 
            Proposition prop = (Proposition) satop.getArg();
            if (!prop.getName().equals(Lexicon.DEFAULT_VAL)) return;
            if (!(satop.getNominal() instanceof NominalVar)) return;
            NominalVar nv = (NominalVar) satop.getNominal();
            SimpleType st = nv.getType();
            // check equality
            if (st.equals(SEMCLASS)) return;
            // otherwise unify types, update nv
            try {
                SimpleType stU = (SimpleType) st.unify(SEMCLASS, null, this.uc);
                nv.setType(stU);
            } catch (UnifyFailure uf) {
                throw new TypePropagationException(st, SEMCLASS);
            }
        }

   
    
    // mod function to replace DEFAULT_VAL with REPLACEMENT
    private void defaultReplacer(Mutable m) {
            if (m instanceof Proposition) {
                Proposition prop = (Proposition) m; 
                if (prop.getName().equals(Lexicon.DEFAULT_VAL)) prop.setAtomName(REPLACEMENT);
            }
            else if (m instanceof FeatureStructure) {
                FeatureStructure fs = (FeatureStructure) m;
                for (Iterator<String> it = fs.getAttributes().iterator(); it.hasNext(); ) {
                    String attr = it.next();
                    Object val = fs.getValue(attr);
                    if (val instanceof SimpleType && 
                        ((SimpleType)val).getName().equals(Lexicon.DEFAULT_VAL))
                    {
                        fs.setFeature(attr, td.getSimpleType(REPLACEMENT));
                    }
                }
            }
        }


    // look up and apply coarts for w to each sign in result
  	private void applyCoarts(Word w, Map<SignSurfaceWords, Sign> result) throws LexException {
          List<Sign> inputSigns = new ArrayList<Sign>(result.values());
          result.clear();
          List<Sign> outputSigns = new ArrayList<Sign>(inputSigns.size());
          // for each surface attr, lookup coarts and apply to input signs, storing results in output signs
          for (Iterator<Pair<String,String>> it = w.getSurfaceAttrValPairs(); it.hasNext(); ) {
              Pair<String,String> p = it.next();
              String attr = (String) p.a;
              if (!lexicon.indexedCoartAttrsContains(attr)) {
              	continue;
              }
              String val = (String) p.b;
              Word coartWord = Word.createWord(wf, attr, val);
              Map<SignSurfaceWords, Sign> coartResult = getSignsFromWord(coartWord, null, null, null);
              for (Iterator<Sign> it2 = coartResult.values().iterator(); it2.hasNext(); ) {
                  Sign coartSign = it2.next();
                  // apply to each input
                  for (int j = 0; j < inputSigns.size(); j++) {
                      Sign sign = inputSigns.get(j);
                      rgd.applyCoart(sign, coartSign, outputSigns);
                  }
              }
              // switch output to input for next iteration
              inputSigns.clear();
              inputSigns.addAll(outputSigns);
              outputSigns.clear();
          }
          // add results back
          for (Sign s : inputSigns) {
          	result.put(new SignSurfaceWords(s), s);
          }
      }
  	  /**
       * For a string of 1 or more surface words, return all of the lexical
       * entries for each word as a list of sign hashes.
       * Tokenization is performed using the configured tokenizer.
       * @param w the words in string format
       *
       * @return a list of sign hashes
       * @exception LexException thrown if word not found
       */
      public List<Map<SignSurfaceWords, Sign>> getEntriesFromWords(String s) throws LexException { 
          List<Map<SignSurfaceWords, Sign>> entries = new ArrayList<Map<SignSurfaceWords, Sign>>();
          List<Word> words = t.tokenize(wf, s);
          for (Iterator<Word> it = words.iterator(); it.hasNext(); ) {
              Word w = it.next();
              Map<SignSurfaceWords, Sign> signs = getSignsFromWord(w);
              if (signs.size() == 0) {
                  throw new LexException("Word not in lexicon: \"" + w +"\"");
              }
              entries.add(signs);
          }
          return entries;
      }
      // get signs using an additional arg for a target rel
      private Collection<Sign> getSignsFromPredAndTargetRel(String pred, String targetRel) {
          
          Set<Word> words = lexicon.getWordsFromPred(pred);
          String specialTokenConst = null;
          
          if (words == null) {
              specialTokenConst = t.getSpecialTokenConstant(t.isSpecialToken(pred));
              if (specialTokenConst == null) return null;
              // lookup words with pred = special token const
              Set<Word> specialTokenWords = lexicon.getWordsFromPred(specialTokenConst);
              // replace special token const with pred
              if (specialTokenWords == null) return null;
              words = new LinkedHashSet<Word>(specialTokenWords.size());
              for (Iterator<Word> it = specialTokenWords.iterator(); it.hasNext(); ) {
                  Word stw = it.next();
                  Word w = Word.createSurfaceWord(wf, stw, pred);
                  words.add(w);
              }
          }
          
          List<Sign> retval = new ArrayList<Sign>();
          for (Iterator<Word> it = words.iterator(); it.hasNext(); ) {
              Word w = it.next();
              try {
                  Map<SignSurfaceWords, Sign> signs = getSignsFromWord(w, specialTokenConst, pred, targetRel);
                  retval.addAll(signs.values());
              }
              // shouldn't happen
              catch (LexException exc) {
                  System.err.println("Unexpected lex exception for word " + w + ": " + exc);
              }
          }
          return retval;
      }

      
     
      
      /**
       * Returns the lexical signs indexed by the given rel, or null if none. 
       */
      public Collection<Sign> getSignsFromRel(String rel) {
          // lookup signs via preds
          Set<String> preds =  lexicon.relsToPredsGet(rel);
          if (preds == null) return null;
          Collection<Sign> retval = getSignsFromRelAndPreds(rel, preds);
          // cache non-null result (if not doing supertagging)
          
          return retval;
      }

      // get signs for rel via preds, or null if none
      private Collection<Sign> getSignsFromRelAndPreds(String rel, Collection<String> preds) {
          List<Sign> retval = new ArrayList<Sign>();
          for (Iterator<String> it = preds.iterator(); it.hasNext(); ) {
              String pred = it.next();
              Collection<Sign> signs = getSignsFromPredAndTargetRel(pred, rel);
              if (signs != null) retval.addAll(signs);
          }
          // return null if none survive filter
          if (retval.size() > 0) return retval;
          else return null;
      }
    
    // returns a macro adder for the given morph item
    private MacroAdder getMacAdder(MorphItem mi) {
        
        // check map
        MacroAdder retval = macAdderMap.get(mi);
        if (retval != null) return retval;
        
        // set up macro adder
        GroupMap<Integer, FeatureStructure> macrosFromLex = new GroupMap<Integer, FeatureStructure>();
        String[] newMacroNames = mi.getMacros();
        List<MacroItem> macroItems = new ArrayList<MacroItem>();
        for (int i=0; i < newMacroNames.length; i++) {
            Set<FeatureStructure> featStrucs = lexicon.getMacros(newMacroNames[i]);
            if (featStrucs != null) {
                for (Iterator<FeatureStructure> fsIt = featStrucs.iterator(); fsIt.hasNext();) {
                    FeatureStructure fs = fsIt.next();
                    macrosFromLex.put(fs.getIndex(), fs);
                }
            }
            MacroItem macroItem = lexicon.getMacroItem(newMacroNames[i]);
            if (macroItem != null) { macroItems.add(macroItem); }
            else { 
                // should be checked earlier too
                System.err.println("Warning: macro " + newMacroNames[i] + 
                    " not found for word '" + mi.getWord() + "'");
            }
        }
        retval = new MacroAdder(macrosFromLex, macroItems, lexicon);
        
        // update map and return
        macAdderMap.put(mi, retval);
        return retval; 
    }
        
    
    //
    // type propagation
    //

    /** Propagates types of nomvars in the given category. */
    public void propagateTypes(Category cat) {
        propagateTypes(cat, null);
    }        
    
    /** Propagates types of nomvars in the given categories. */
    public void propagateTypes(Category cat, Category cat2) {
        try {
            nomvarMap.clear();
            cat.mutateAll(this::nomvarTypePropagater);
            if (cat2 != null) cat2.mutateAll(this::nomvarTypePropagater);
            cat.mutateAll(this::nomvarTypePropagater);
            if (cat2 != null) cat2.mutateAll(this::nomvarTypePropagater);
        } catch (TypePropagationException tpe) {
	            System.err.println(
	                "Warning: unable to unify types '" + tpe.st1 + "' and '" + tpe.st2 + 
	                "' in cat: \n" + cat
	            );
	            tpe.printStackTrace();
	            if (cat2 != null) System.err.println("and cat: \n" + cat2);
        }
    }
        
	
    
    // exception for unification failures in propagating types
    private class TypePropagationException extends RuntimeException {
		private static final long serialVersionUID = 1L;
		SimpleType st1; SimpleType st2;
        TypePropagationException(SimpleType st1, SimpleType st2) {
            this.st1 = st1; this.st2 = st2;
        }
    }
    
    // mod function to propagate nomvar types; 
    // needs to be called twice after clearing nomvarMap
    private void nomvarTypePropagater(Mutable m) {
        if (m instanceof NominalVar) {
            NominalVar nv = (NominalVar) m;
            SimpleType st = nv.getType();
            SimpleType st0 = nomvarMap.get(nv);
            // add type to map if no type found
            if (st0 == null) { nomvarMap.put(nv.getName(), st); return; }
            // check equality
            if (st.equals(st0)) return;
            // otherwise unify types, update nv and map
            try {
                SimpleType stU = (SimpleType) st.unify(st0, null, this.uc);
                nv.setType(stU);
                nomvarMap.put(nv.getName(), stU);
            } catch (UnifyFailure uf) {
                throw new TypePropagationException(st, st0);
            }
        }
     }
    

    //
    // distributive attribute propagation
    //

//    /**
//     * Returns the list of distributive attributes, or null if none.
//     */
//    public String[] getDistributiveAttrs() { return lexicon._distributiveAttrs; }
    
    /**
     * Gathers and propagates the unique values of each 
     * distributive attribute.
     */
    public void propagateDistributiveAttrs(Category cat) {
        propagateDistributiveAttrs(cat, null);
    }
    
    /**
     * Gathers and propagates the unique values of each 
     * distributive attribute.
     */
    public void propagateDistributiveAttrs(Category cat, Category cat2) {
        if (lexicon.numDistrAttrs() == 0) return;
        resetDistrAttrVals();
        cat.applyToAll(this::gatherDistrAttrVals);
        if (cat2 != null) { cat2.applyToAll(this::gatherDistrAttrVals); }
        cat.applyToAll(this::propagateUniqueDistrAttrVals);
        if (cat2 != null) { cat2.applyToAll(this::propagateUniqueDistrAttrVals); }
    }
    
    
	private void resetDistrAttrVals() {
        if (distrAttrVals == null) { 
            distrAttrVals = new ArrayList<List<Object>>(lexicon.numDistrAttrs());
            for (int i = 0; i < distrAttrVals.size(); i++) {
                distrAttrVals.add(i, new ArrayList<Object>(3));
            }
            return;
        }
        for (int i = 0; i < distrAttrVals.size(); i++) {
            distrAttrVals.get(i).clear();
        }
    }

	private void gatherDistrAttrVals(Category c) {
        if (!(c instanceof AtomCat)) return;
        FeatureStructure fs = c.getFeatureStructure();
        if (fs == null) return;
        for (int i = 0; i < lexicon.numDistrAttrs(); i++) {
            String attr = lexicon.getDistrAttr(i);
            Object val = fs.getValue(attr);
            if (val != null && !distrAttrVals.get(i).contains(val)) { 
                distrAttrVals.get(i).add(val); 
            }
        }
    }
    // propagates unique values for each distributive attr
    private void propagateUniqueDistrAttrVals(Category c) {
            if (!(c instanceof AtomCat)) return;
            FeatureStructure fs = c.getFeatureStructure();
            if (fs == null) return;
            for (int i = 0; i < lexicon.numDistrAttrs(); i++) {
                if (distrAttrVals.get(i).size() != 1) continue;
                Object distVal = distrAttrVals.get(i).get(0);
                String attr = lexicon.getDistrAttr(i);
                Object val = fs.getValue(attr);
                if (val == null) {
                    fs.setFeature(attr, UnifyControl.copy(distVal));
                }
            }
     }
    /** Expands inheritsFrom links to feature equations for those features not explicitly listed. */ 
    public void expandInheritsFrom(Category cat) {
        expandInheritsFrom(cat, null);
    }
    /** Expands inheritsFrom links to feature equations for those features not explicitly listed. */ 
    public void expandInheritsFrom(Category cat, Category cat2) {
        // index feature structures
        featStrucMap.clear();
        cat.applyToAll(this::indexFeatStrucs);
        if (cat2 != null) { cat2.applyToAll(this::indexFeatStrucs); }
        // add feature eqs 
        cat.applyToAll(this::doInheritsFrom);
        if (cat2 != null) { cat2.applyToAll(this::doInheritsFrom); }
    }
    
    
    // fills in featStrucMap for a category
    private void indexFeatStrucs(Category c) {
            FeatureStructure fs = c.getFeatureStructure();
            if (fs != null && fs.getIndex() != 0)
                featStrucMap.put(fs.getIndex(), fs);
    }

    // adds feature equations to percolate attributes from inheritsFrom feature 
    // structure, except for any attributes already present
    private void doInheritsFrom(Category c) {
            // get feature structures
            if (!(c instanceof AtomCat)) return;
            String type = ((AtomCat)c).getType();
            FeatureStructure fs = c.getFeatureStructure();
            GFeatStruc gfs = (GFeatStruc) fs;
            if (gfs == null || gfs.getInheritsFrom() == 0) return;
            int inhf = gfs.getInheritsFrom();
            FeatureStructure inhfFS = featStrucMap.get(inhf);
            if (inhfFS != null) {
                // copy values of features from inhfFS not already present
                for (Iterator<String> it = inhfFS.getAttributes().iterator(); it.hasNext(); ) {
                    String att = it.next(); 
                    if (gfs.hasAttribute(att)) continue;
                    gfs.setFeature(att, UnifyControl.copy(inhfFS.getValue(att)));
                }
                // for each possible attr used with this type and not already present, 
                // add feature equation
                Collection<String> attrs = lexicon.getCatsToAttrs(type);
                if (attrs == null) return;
                for (Iterator<String> it = attrs.iterator(); it.hasNext(); ) {
                    String att = it.next(); 
                    if (gfs.hasAttribute(att)) continue;
                    String varName = att.toUpperCase() + inhf;
                    if (lexicon.lfattrsContains(att)) {
                        gfs.setFeature(att, new HyloVar(lexicon, td, varName));
                        inhfFS.setFeature(att, new HyloVar(lexicon, td, varName));
                    }
                    else {
                        gfs.setFeature(att, new GFeatVar(td, varName));
                        inhfFS.setFeature(att, new GFeatVar(td, varName));
                    }
                }
            }
            else {
                System.err.println(
                    "Warning: no feature structure with inheritsFrom index of " + inhf + 
                    " found in category " + c
                );
            }
        }
   

    
}
