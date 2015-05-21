///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003 Jason Baldridge and University of Edinburgh (Michael White)
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import lexicon.LexicalData;
import lexicon.Lexicon;
import lexicon.Tokenizer;
import synsem.AtomCat;
import synsem.Category;
import synsem.ComplexCat;
import synsem.LF;
import synsem.Sign;
import unify.FeatureStructure;
import unify.GFeatStruc;
import unify.Substitution;
import unify.UnifyControl;
import unify.UnifyFailure;
import unify.Variable;

/**
 * Implements some default behavior for Rule objects.
 *
 * @author  Jason Baldridge
 * @author  Michael White
 * @version $Revision: 1.18 $, $Date: 2009/12/21 03:27:18 $
 */
public abstract class AbstractRule implements Rule, Serializable {

	private static final long serialVersionUID = 1L;

    protected String name;
    
    protected final UnifyControl uc;
    protected final LexicalData lex;
    protected final Lexicon l;
    protected RuleGroupData ruleGroup;
    protected final Tokenizer t;
    
    protected AbstractRule(UnifyControl uc, LexicalData lex, Lexicon l, Tokenizer t) {
    	this.uc = uc;
    	this.lex = lex;
    	this.l = l;
    	this.t = t;
    }
    
    /** Reusable list of head cats, one for each result. */
    protected List<Category> headCats = new ArrayList<Category>();
    

    /** Applies the rule to the given input signs, adding to the given list of results. */
    public void applyRule(Sign[] inputs, List<Sign> results) {

        if (inputs.length != arity()) { // shouldn't happen
            throw new RuntimeException("Inputs must have length " + arity());
        }

        Category[] cats = new Category[inputs.length];
        for (int i=0; i < cats.length; i++) {
            cats[i] = inputs[i].getCategory();
        }

        try {
            List<Category> resultCats = applyRule(cats);
            if (resultCats.isEmpty()) return;
            
            for (int i=0; i < resultCats.size(); i++) {
            	Category catResult = resultCats.get(i);
                distributeTargetFeatures(catResult);
                Category headCat = headCats.get(i);
                Sign lexHead = inputs[0].getLexHead();
                for (int j=0; j < inputs.length; j++) {
                	if (inputs[j].getCategory().equals(headCat)) lexHead = inputs[j].getLexHead();
                }
                Sign sign = Sign.createDerivedSign(ruleGroup, t, catResult, inputs, this, lexHead);
                results.add(sign);
            }
        } catch (UnifyFailure uf) {}
    }
    
    
    // target cat's feature structure
    private GFeatStruc targetFS = null;
    
	private void distributeTargetFeaturesFcn(Category c) {
        if (!(c instanceof AtomCat)) return;
        FeatureStructure fs = c.getFeatureStructure();
        if (fs == null) return;
        if (fs.equals(targetFS)) return;
        for (int i = 0; i < l.numDistrAttrs(); i++) {
            Object targetVal = targetFS.getValue(l.getDistrAttr(i));
            if (targetVal != null && !(targetVal instanceof Variable)) {
                fs.setFeature(l.getDistrAttr(i), UnifyControl.copy(targetVal));
            }
        }
    }
	protected void distributeTargetFeatures(Category cat) {
        if (!(cat instanceof ComplexCat)) return;
        ComplexCat complexCat = (ComplexCat) cat;
        Category targetCat = (Category) complexCat.getTarget();
        targetFS = (GFeatStruc) targetCat.getFeatureStructure();
        if (targetFS == null) return;
        cat.applyToAll(this::distributeTargetFeaturesFcn);
    }
    
    
    
    /**
     * The number of arguments this rule takes.  For example, the arity of the
     * forward application rule of categorial grammar (X/Y Y => Y) is 2.
     *
     * @return the number of arguments this rule takes
     **/
    public abstract int arity();

    /**
     * Apply this rule to some input categories.
     * @param inputs the input categories to try to combine
     * @return the categories resulting from using this rule to combine the
     *         inputs
     * @exception UnifyFailure if the inputs cannot be combined by this rule
     **/
    public abstract List<Category> applyRule(Category[] inputs) throws UnifyFailure;

    
    public String name() {
        return name;
    }
    
    /**
     * Returns the rule group which contains this rule.
     */
    public RuleGroupData getRuleGroup() { return ruleGroup; }
    
    /**
     * Sets this rule's rule group.
     */
    public void setRuleGroup(RuleGroupData ruleGroup) { 
    	this.ruleGroup = ruleGroup; 
    }

    
    /** Appends, fills, sorts and checks the LFs from cats 1 and 2 into the result cat. */
    protected void appendLFs(Category cat1, Category cat2, Category result, Substitution sub) 
        throws UnifyFailure
    {
        LF lf = HyloHelper.append(l, cat1.getLF(), cat2.getLF());
        if (lf != null) {
            lf = (LF) lf.fill(uc, sub);
            HyloHelper.sort(l, lf);
            HyloHelper.check(lf);
        }
        result.setLF(lf);
    }
}

