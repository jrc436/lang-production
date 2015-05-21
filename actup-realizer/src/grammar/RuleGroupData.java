package grammar;

import hylo.HyloHelper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import lexicon.IWordFactory;
import lexicon.LexicalData;
import lexicon.Lexicon;
import lexicon.Tokenizer;
import synsem.Category;
import synsem.LF;
import synsem.Sign;
import unify.UnifyControl;
import unify.UnifyFailure;
import util.GroupMap;

public class RuleGroupData {
		private final IWordFactory wf;
		private final Tokenizer t;
		
		private final boolean dynamicCombos;
		 // rules
	    private final List<Rule> unaryRules = new ArrayList<Rule>();
	    private final List<Rule> binaryRules = new ArrayList<Rule>();
	    
	    // observed supercat combos (for which complete rule combos are known)
	    private final Map<SupercatCombo, SupercatRuleCombo> supercatCombosSeen;
	    private final Map<SupercatCombo, SupercatRuleCombo> supercatRuleCombos;
	    
	    /** Returns the unary rules. */
	    public List<Rule> getUnaryRules() { return unaryRules; }

	    /** Returns the binary rules. */
	    public List<Rule> getBinaryRules() { return binaryRules; }

	    // maps of type changing rules by their semantics
	    private final GroupMap<String,TypeChangingRule> predsToRules = new GroupMap<String,TypeChangingRule>();
	    private final GroupMap<String,TypeChangingRule> relsToRules = new GroupMap<String,TypeChangingRule>();
	    
	    // rule for use in applying coarticulations
	    private final BackwardApplication bapp;

	    // glue rule
	    private final GlueRule glueRule;
	    
	    
		public RuleGroupData(IWordFactory wf, TypesData td, LexicalData lex, UnifyControl uc, Lexicon l, RuleGroup rg, Tokenizer t) {
			this.t = t;
			this.dynamicCombos = rg.usingDynamicCombos();
			this.wf = wf;
			bapp = new BackwardApplication(uc, lex, l, t);
		    bapp.setRuleGroup(this);
		    glueRule = new GlueRule(uc, lex, l, td, t);  
		    supercatCombosSeen = new LinkedHashMap<SupercatCombo, SupercatRuleCombo>();
		    supercatRuleCombos = rg.copyInitialCombos();
		    
		    for (RuleStruct rs : rg.copyRuleStructs()) {
		    	if (rs instanceof TypeChangingRuleStruct) {
		    		addRule(createTypeChangingRule((TypeChangingRuleStruct)rs, uc, lex));
		    	}
		    	else {
		    		addRule(rs.getRuleFromStruct(uc, lex));
		    	}
		    }
		}
		public RuleGroupData(IWordFactory wf, TypesData td, LexicalData lex, UnifyControl uc, Lexicon l, RuleGroupData rgd, Tokenizer t) {
			this.t = t;
			this.wf = wf;
			bapp = new BackwardApplication(uc, lex, l, t);
		    bapp.setRuleGroup(this);
		    glueRule = new GlueRule(uc, lex, l, td, t);  
		    supercatCombosSeen = new LinkedHashMap<SupercatCombo, SupercatRuleCombo>(rgd.supercatCombosSeen);
		    supercatRuleCombos = new LinkedHashMap<SupercatCombo, SupercatRuleCombo>(rgd.supercatRuleCombos);
		    this.dynamicCombos = rgd.dynamicCombos;
		}
		public Rule createTypeChangingRule(TypeChangingRuleStruct tcrs, UnifyControl uc, LexicalData lex) {
	        lex.propagateTypes(tcrs.result, tcrs.arg);
	        lex.propagateDistributiveAttrs(tcrs.result, tcrs.arg);
	        lex.expandInheritsFrom(tcrs.result, tcrs.arg);

	        return tcrs.getRuleFromStruct(uc, lex);
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
	            // shouldn't happenRuleGroup
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
	    /** Returns the type changing rule with the given name, or null if none. */
	    public TypeChangingRule getTypeChangingRule(String name) {
	    	for (Rule rule : unaryRules) {     
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
	    		SupercatRuleCombo rep = supercatCombosSeen.get(new SupercatCombo(supertag));
	    		if (rep == null) dynamicCombosUpdate = true;
	    		else if (!rep.hasRule()) skip = true;
	        }
	        // skip if possible
	        if (skip) return results;
	        // try each rule 
	        for (Rule r : unaryRules) {
	        	// filter on observed supercat-rule combos, if any, if not updating
	        	if (!dynamicCombosUpdate && supercatRuleCombos != null) {
	        		if (!supercatRuleCombos.containsKey(new SupercatCombo(supertag, r.name()))) { continue; }
	        	}
	        	// if updating combos, apply rule and record results
	        	if (dynamicCombosUpdate) {
	        		int prevsize = results.size();
	            	((AbstractRule)r).applyRule(inputs, results);
	            	// update upon success
	            	if (results.size() > prevsize) {
	            		SupercatRuleCombo newCombo = null;
	            		if (!supercatRuleCombos.containsKey(new SupercatCombo(supertag, r.name()))) { 
	            			newCombo = new SupercatRuleCombo(supertag, r.name());
	            			supercatRuleCombos.put(newCombo.getSupercat(), newCombo);
	            		}
	            		if (!supercatCombosSeen.containsKey(new SupercatCombo(supertag, r.name()))) {
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
	    		if (!supercatCombosSeen.containsKey(new SupercatCombo(supertag))) {
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
	    		SupercatRuleCombo rep = supercatCombosSeen.get(new SupercatCombo(supertag1, supertag2));
	    		if (rep == null) dynamicCombosUpdate = true;
	    		else if (rep.hasRule()) skip = true;
	        }
	        // skip if possible
	        if (skip) return results;
	        // try each rule
	        for (Rule r : binaryRules) {
	        	// filter on observed supercat-rule combos, if any, if not updating
	        	if (!dynamicCombosUpdate && supercatRuleCombos != null) {
	        		if (!supercatRuleCombos.containsKey(new SupercatCombo(supertag1, supertag2))) { continue; }
	        	}
	        	// if updating combos, apply rule and record results
	        	if (dynamicCombosUpdate) {
	        		int prevsize = results.size();
	            	((AbstractRule)r).applyRule(inputs, results);
	            	// update upon success
	            	if (results.size() > prevsize) {
	            		SupercatRuleCombo newCombo = new SupercatRuleCombo(supertag1, supertag2, r.name());
	            		if (!supercatRuleCombos.containsKey(newCombo.getSupercat())) {
	            			supercatRuleCombos.put(newCombo.getSupercat(), newCombo);
	            		}
	            		if (!supercatCombosSeen.containsKey(newCombo.getSupercat())) {
	                		supercatCombosSeen.put(newCombo.getSupercat(), newCombo);
	            		}
	            	}
	        	}
	        	// otherwise just apply rule
	        	else ((AbstractRule)r).applyRule(inputs, results);
	        }
	        // if updating combos and none succeeded, add one with null rule
	        if (dynamicCombosUpdate) {
	        	SupercatRuleCombo newCombo = new SupercatRuleCombo(supertag1, supertag2, null);
	    		if (!supercatCombosSeen.containsKey(newCombo.getSupercat())) {        		
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
	                Sign sign = Sign.createCoartSign(wf, this, t, catResult, lexSign, coartSign);
	                results.add(sign);
	            }
	        } catch (UnifyFailure uf) {}
	    }
}
