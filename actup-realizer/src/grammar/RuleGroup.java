///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-6 Jason Baldridge, Gann Bierner and 
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

package grammar;

import hylo.HyloHelper;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.jdom.Element;

import synsem.CatReader;
import synsem.Category;
import synsem.LF;
import synsem.Sign;
import unify.UnifyFailure;
import util.GroupMap;
import util.XmlScanner;

/**
 * A set of rules for combining categories.
 * Observed rule combos can be cached, either statically or dynamically.
 *
 * During deserialization, the grammar is set to the current grammar, 
 * and supercat rule combos are borrowed from the current grammar's rule group.
 * 
 * @author      Jason Baldridge
 * @author      Gann Bierner
 * @author      Michael White
 * @version     $Revision: 1.32 $, $Date: 2011/06/07 05:12:01 $
 */
public class RuleGroup implements Serializable {

	private static final long serialVersionUID = -6240266013357142289L;

	/** The grammar that this rule group is part of. */
    private final Grammar grammar;
    
    // rules
    private List<Rule> unaryRules = new ArrayList<Rule>();
    private List<Rule> binaryRules = new ArrayList<Rule>();

    // maps of type changing rules by their semantics
    private GroupMap<String,TypeChangingRule> predsToRules = new GroupMap<String,TypeChangingRule>();
    private GroupMap<String,TypeChangingRule> relsToRules = new GroupMap<String,TypeChangingRule>();
    
    // rule for use in applying coarticulations
    private BackwardApplication bapp;

    // glue rule
    private GlueRule glueRule;
    
    // observed supercat-rule combos
    private Map<SupercatCombo, SupercatRuleCombo> supercatRuleCombos = null;
    
    // observed supercat combos (for which complete rule combos are known)
    private Map<SupercatCombo, SupercatRuleCombo> supercatCombosSeen = null;
    
    // reusable combo for checking presence
    private SupercatRuleCombo combo = new SupercatRuleCombo("dummy", "dummy");
    
    // flag for whether observed supercat combos is determined dynamically
    private boolean dynamicCombos = false;
    
    /**
     * Constructs an empty rule group for the given grammar.
     */
    public RuleGroup(Grammar grammar) {
        this.grammar = grammar;
        bapp = new BackwardApplication(grammar);
        bapp.setRuleGroup(this);
        glueRule = new GlueRule(grammar);     
    }
    
    /**
     * Constructs a rule group from the given URL, for 
     * the given grammar.
     */
    public RuleGroup(URL url, Grammar grammar) throws IOException {
        this(grammar);
        XmlScanner ruleScanner = new XmlScanner() {
        	public void handleElement(Element ruleEl) {
                String active = ruleEl.getAttributeValue("active");
                if (active == null || active.equals("true")) {
                    try { addRule(readRule(ruleEl)); }
                    catch (RuntimeException exc) {
                        System.err.println("Skipping rule: " + ruleEl.getAttributeValue("name"));
                        System.err.println(exc.toString());
                    }
                }
        	}
        };
        ruleScanner.parse(url);
    }
    
    // reads in a rule
    private Rule readRule(Element ruleEl) {
        Rule r;
        String type = ruleEl.getName();
        if (type.equals("application")) {
            String dir = ruleEl.getAttributeValue("dir");
            if (dir.equals("forward")) {
                r = new ForwardApplication(grammar);
            } else {
                r = new BackwardApplication(grammar);
            }
        } else if (type.equals("composition")) {
            String dir = ruleEl.getAttributeValue("dir");
            String harmonic = ruleEl.getAttributeValue("harmonic");
            boolean isHarmonic = new Boolean(harmonic).booleanValue();
            if (dir.equals("forward")) {
                r = new ForwardComposition(grammar, isHarmonic);
            } else {
                r = new BackwardComposition(grammar, isHarmonic);
            }
        } else if (type.equals("substitution")) {
            String dir = ruleEl.getAttributeValue("dir");
            String harmonic = ruleEl.getAttributeValue("harmonic");
            boolean isHarmonic = new Boolean(harmonic).booleanValue();
            if (dir.equals("forward")) {
                r = new ForwardSubstitution(grammar, isHarmonic);
            } else {
                r = new BackwardSubstitution(grammar, isHarmonic);
            }
        } else if (type.equals("typeraising")) {
            String dir = ruleEl.getAttributeValue("dir");
            String useDollar = ruleEl.getAttributeValue("useDollar");
            boolean addDollar = new Boolean(useDollar).booleanValue();
            Category arg = null;
            Element argElt = ruleEl.getChild("arg");
            if (argElt != null) {
                arg = CatReader.getCat(grammar, (Element)argElt.getChildren().get(0));
            }
            Category result = null;
            Element resultElt = ruleEl.getChild("result");
            if (resultElt != null) {
                result = CatReader.getCat(grammar, (Element)resultElt.getChildren().get(0));
            }
            if (dir.equals("forward")) {
                r = new ForwardTypeRaising(grammar, addDollar, arg, result);
            } else {
                r = new BackwardTypeRaising(grammar, addDollar, arg, result);
            }
        } else if (type.equals("typechanging")) {
            r = readTypeChangingRule(ruleEl);
        } else {
            throw new RuntimeException("Invalid element in rules: " + type);
        }
        return r;
    }
    
    // reads in a type changing rule
    private Rule readTypeChangingRule(Element ruleEl) {
        
        String rname = ruleEl.getAttributeValue("name");
        Element argCatElt = (Element)ruleEl.getChild("arg").getChildren().get(0);
        Category arg = CatReader.getCat(grammar, argCatElt);
        Element resultCatElt = (Element)ruleEl.getChild("result").getChildren().get(0);
        Element lfElt = resultCatElt.getChild("lf");
        Category result = CatReader.getCat(grammar, resultCatElt);
        LF firstEP = null;
        if (lfElt != null) {
            firstEP = HyloHelper.firstEP(grammar, HyloHelper.getLF(grammar, lfElt));
        }
        
        grammar.lexicon.propagateTypes(result, arg);
        grammar.lexicon.propagateDistributiveAttrs(result, arg);
        grammar.lexicon.expandInheritsFrom(result, arg);

        return new TypeChangingRule(grammar, arg, result, rname, firstEP);
    }

    /**
     * Sets the dynamic combos flag to the given value, controlling whether the 
     * observed supercat combos is determined dynamically.
     */
    public void setDynamicCombos(boolean dynamic) {
    	this.dynamicCombos = dynamic;
    	if (!dynamicCombos) supercatCombosSeen = null;
    	else if (dynamicCombos) {
    		if (supercatCombosSeen == null) supercatCombosSeen = new LinkedHashMap<SupercatCombo, SupercatRuleCombo>();
    		if (supercatRuleCombos == null) supercatRuleCombos = new LinkedHashMap<SupercatCombo, SupercatRuleCombo>();
    	}
    }
    
    /**
     * Returns the dynamic combos flag.
     */
    public boolean getDynamicCombos() { return dynamicCombos; }
    
    /** 
     * Loads the observed supercat-rule combos, for filtering. 
     * Only file URLs are supported at present.
     * Missing files are ignored. 
     **/
    public void loadSupercatRuleCombos(URL url) throws IOException {
    	supercatRuleCombos = new LinkedHashMap<SupercatCombo, SupercatRuleCombo>();
    	File combosFile = new File(url.getFile());
    	if (!combosFile.exists()) return;
    	System.out.println("Loading supercat combos from " + url.getFile());
    	BufferedReader in = new BufferedReader(new FileReader(combosFile));
    	String line;
    	while ((line = in.readLine()) != null) {
    		String[] tokens = line.split("\\s");
    		if (tokens.length < 2) {
    			System.err.println("Warning: skipping supercat-rule combo with fewer than two tokens: " + line);
    			continue;
    		}
    		if (tokens.length == 2) {
    			SupercatRuleCombo toAdd = new SupercatRuleCombo(tokens[0], tokens[1]);
    			supercatRuleCombos.put(toAdd.getSupercat(), toAdd);
    		}
    		else {
	    		if (tokens.length > 3) {
	    			System.err.println("Warning: ignoring extra tokens (beyond 3rd) in supercat-rule combo: " + line);
	    		}
	    		SupercatRuleCombo toAdd = new SupercatRuleCombo(tokens[0], tokens[1], tokens[2]);
    			supercatRuleCombos.put(toAdd.getSupercat(), toAdd);
    		}
    	}
    	in.close();
    }
    
    
    /** Borrows the observed supercat-rule combos from the given rule group. */
    public void borrowSupercatRuleCombos(RuleGroup ruleGroup) {
    	supercatRuleCombos = ruleGroup.supercatRuleCombos;
    	supercatCombosSeen = ruleGroup.supercatCombosSeen;
    }
    
    
    /** Adds the given rule. */
    public void addRule(Rule r) {
       r.setRuleGroup(this);
        if (r instanceof TypeChangingRule) {
            unaryRules.add(r);
            index((TypeChangingRule)r);
        }
        else if (r.arity() == 1) { unaryRules.add(r); } 
        else if (r.arity() == 2) { binaryRules.add(r); } 
        else {
            // shouldn't happen
            throw new RuntimeException("Can't determine arity of rule: " + r);
        }
    }

    // indexes type changing rules by preds and rels
    private void index(TypeChangingRule rule) {
        LF firstEP = rule.getFirstEP();
        if (firstEP == null) { return; }
        String pred = HyloHelper.getLexPred(firstEP);
        if (pred != null) { 
            predsToRules.put(pred, rule); 
            return; 
        }
        String rel = HyloHelper.getRel(firstEP);
        if (rel != null) { 
            relsToRules.put(rel, rule);
        }
    }
    
    
    /** Returns the unary rules. */
    public List<Rule> getUnaryRules() { return unaryRules; }

    /** Returns the binary rules. */
    public List<Rule> getBinaryRules() { return binaryRules; }

    /** Returns the type changing rule with the given name, or null if none. */
    public TypeChangingRule getTypeChangingRule(String name) {
        for (Iterator<Rule> it = unaryRules.iterator(); it.hasNext(); ) {
            Object rule = it.next();
            if (rule instanceof TypeChangingRule) {
                TypeChangingRule tcr = (TypeChangingRule) rule;
                if (tcr.name().equals(name)) return tcr;
            }
        }
        return null;
    }
    
    /**
     * Returns the type changing rules indexed by the given lexical predicate. 
     * The type changing rules are indexed by their first elementary predication.
     */
    public Collection<TypeChangingRule> getRulesForPred(String pred) {
        return predsToRules.get(pred);
    }
    
    /**
     * Returns the type changing rules indexed by the given relation.
     * The type changing rules are indexed by their first elementary predication.
     */
    public Collection<TypeChangingRule> getRulesForRel(String rel) {
        return relsToRules.get(rel);
    }
    
    
    /** Applies the unary rules to the given input sign, returning the list of results. */
    public List<Sign> applyUnaryRules(Sign input) {
    	Sign[] inputs = { input };
        List<Sign> results = new ArrayList<Sign>(2);
        String supertag = input.getCategory().getSupertag();
        // check whether dynamic combos update required, or whether rules can be skipped
        boolean dynamicCombosUpdate = false;
        boolean skip = false;
        if (dynamicCombos) {
    		combo.setCombo(supertag, null);
    		SupercatRuleCombo rep = supercatCombosSeen.containsValue(combo) ? combo : null;
    		if (rep == null) dynamicCombosUpdate = true;
    		else if (rep.rule == null) skip = true;
        }
        // skip if possible
        if (skip) return results;
        // try each rule 
        for (Rule r : unaryRules) {
        	// filter on observed supercat-rule combos, if any, if not updating
        	if (!dynamicCombosUpdate && supercatRuleCombos != null) {
        		combo.setCombo(supertag, r.name());
        		if (!supercatRuleCombos.containsKey(combo.getSupercat())) { continue; }
        	}
        	// if updating combos, apply rule and record results
        	if (dynamicCombosUpdate) {
        		int prevsize = results.size();
            	((AbstractRule)r).applyRule(inputs, results);
            	// update upon success
            	if (results.size() > prevsize) {
            		SupercatRuleCombo newCombo = null;
            		combo.setCombo(supertag, r.name());
            		if (!supercatRuleCombos.containsKey(combo.getSupercat())) { 
            			newCombo = new SupercatRuleCombo(supertag, r.name());
            			supercatRuleCombos.put(newCombo.getSupercat(), newCombo);
            		}
            		if (!supercatCombosSeen.containsKey(combo.getSupercat())) {
            			if (newCombo == null) newCombo = new SupercatRuleCombo(supertag, r.name());
                		supercatCombosSeen.put(newCombo.getSupercat(), newCombo);
            		}
            	}
        	}
        	// otherwise just apply rule
        	else ((AbstractRule)r).applyRule(inputs, results);
        }
        // if updating combos and none succeeded, add one with null rule
        if (dynamicCombosUpdate) {
    		combo.setCombo(supertag, null);
    		if (!supercatCombosSeen.containsKey(combo.getSupercat())) {
    			SupercatRuleCombo newCombo = new SupercatRuleCombo(supertag, null);
    			supercatCombosSeen.put(newCombo.getSupercat(), newCombo);
    		}
        }
        // done
        return results;
    }
    
    /** Applies the binary rules to the given input signs, returning the list of results. */
    public List<Sign> applyBinaryRules(Sign input1, Sign input2) {
    	Sign[] inputs = { input1, input2 };
        List<Sign> results = new ArrayList<Sign>(2);
		String supertag1 = input1.getCategory().getSupertag();
		String supertag2 = input2.getCategory().getSupertag();
        // check whether dynamic combos update required, or whether rules can be skipped
        boolean dynamicCombosUpdate = false;
        boolean skip = false;
        if (dynamicCombos) {
    		combo.setCombo(supertag1, supertag2, null);
    		SupercatRuleCombo rep = supercatCombosSeen.containsKey(combo.getSupercat()) ? combo : null;
    		if (rep == null) dynamicCombosUpdate = true;
    		else if (rep.rule == null) skip = true;
        }
        // skip if possible
        if (skip) return results;
        // try each rule
        for (Rule r : binaryRules) {
        	// filter on observed supercat-rule combos, if any, if not updating
        	if (!dynamicCombosUpdate && supercatRuleCombos != null) {
        		combo.setCombo(supertag1, supertag2, r.name());
        		if (!supercatRuleCombos.containsKey(combo.getSupercat())) { continue; }
        	}
        	// if updating combos, apply rule and record results
        	if (dynamicCombosUpdate) {
        		int prevsize = results.size();
            	((AbstractRule)r).applyRule(inputs, results);
            	// update upon success
            	if (results.size() > prevsize) {
            		SupercatRuleCombo newCombo = null;
            		combo.setCombo(supertag1, supertag2, r.name());
            		if (!supercatRuleCombos.containsKey(combo.getSupercat())) { 
            			newCombo = new SupercatRuleCombo(supertag1, supertag2, r.name());
            			supercatRuleCombos.put(newCombo.getSupercat(), newCombo);
            		}
            		if (!supercatCombosSeen.containsKey(combo.getSupercat())) {
            			if (newCombo == null) newCombo = new SupercatRuleCombo(supertag1, supertag2, r.name());
                		supercatCombosSeen.put(newCombo.getSupercat(), newCombo);
            		}
            	}
        	}
        	// otherwise just apply rule
        	else ((AbstractRule)r).applyRule(inputs, results);
        }
        // if updating combos and none succeeded, add one with null rule
        if (dynamicCombosUpdate) {
    		combo.setCombo(supertag1, supertag2, null);
    		if (!supercatCombosSeen.containsKey(combo.getSupercat())) {
        		SupercatRuleCombo newCombo = new SupercatRuleCombo(supertag1, supertag2, null);
    			supercatCombosSeen.put(newCombo.getSupercat(), newCombo);
    		}
        }
        // done
        return results;
    }
    
    
    /** Applies the glue rule to the given input signs, returning the list of results. */
    public List<Sign> applyGlueRule(Sign input1, Sign input2) {
    	Sign[] inputs = { input1, input2 };
        List<Sign> results = new ArrayList<Sign>(1);
    	glueRule.applyRule(inputs, results);
        return results;
    }

    
    /** Applies the coarticulation to the given sign, adding the result (if any) to the given ones. */
    public void applyCoart(Sign lexSign, Sign coartSign, List<Sign> results) {

        Category[] cats = new Category[] { lexSign.getCategory(), coartSign.getCategory() }; 

        try {
            List<Category> resultCats = bapp.applyRule(cats);
            if (resultCats.isEmpty()) return;
            
            for (Iterator<Category> it = resultCats.iterator(); it.hasNext();) {
                Category catResult = it.next();
                bapp.distributeTargetFeatures(catResult);
                Sign sign = Sign.createCoartSign(grammar, catResult, lexSign, coartSign);
                results.add(sign);
            }
        } catch (UnifyFailure uf) {}
    }
}
