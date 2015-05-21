///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-6 Jason \Baldridge, Gann Bierner and 
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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import lexicon.Lexicon;
import lexicon.Tokenizer;

import org.jdom.Element;

import synsem.CatReader;
import synsem.Category;
import synsem.LF;
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

//RuleGroup is immutable, and read from standard in. RuleGroupData handles all mutable structures.
public class RuleGroup implements Serializable {

	private static final long serialVersionUID = -6240266013357142289L;

    private final Lexicon l;
   
    private TypesData td;
    
   
   private final Tokenizer t;
    
    //it's ok to just copy these, because the elements themselves are very immutable.
    private final List<RuleStruct> rulesToAdd = new ArrayList<RuleStruct>();
    protected List<RuleStruct> copyRuleStructs() {
    	return new ArrayList<RuleStruct>(rulesToAdd);
    }
   

    private final Map<SupercatCombo, SupercatRuleCombo> initialSupercatCombos;
    protected Map<SupercatCombo, SupercatRuleCombo> copyInitialCombos() {
    	return new LinkedHashMap<SupercatCombo, SupercatRuleCombo>(initialSupercatCombos);
    }
    
   
    
    // flag for whether observd supercat combos is determined dynamically
    private final boolean dynamicCombos;
    public boolean usingDynamicCombos() { return dynamicCombos; }
    
    /**
     * Constructs an empty rule group for the given grammar.
     * @param dynamic TODO
     * @param t TODO
     */
    private RuleGroup(Lexicon l, TypesData td, boolean dynamic, Map<SupercatCombo, SupercatRuleCombo> combos, Tokenizer t) {  
        this.l = l;
        this.t = t;
        this.dynamicCombos = dynamic;
        this.td = td;
    	initialSupercatCombos = combos;
    }
    public RuleGroup(URL rulesUrl, Lexicon l, TypesData td, boolean dynamic, Tokenizer t) throws IOException {
    	this(rulesUrl, null, l, td, dynamic, t);
    }
    public RuleGroup(URL rulesUrl, Lexicon l, TypesData td, URL combosUrl, Tokenizer t) throws IOException {
    	this(rulesUrl, combosUrl, l, td, true, t);
    }
    public RuleGroup(URL rulesUrl, Lexicon l, TypesData td, Tokenizer t) throws IOException {
    	this(rulesUrl, null, l, td, false, t);
    }
    
    /**
     * Constructs a rule group from the given URL, for 
     * the given grammar.
     * @param dynamic TODO
     * @param t TODO
     */
    public RuleGroup(URL rulesUrl, URL combosURL, Lexicon l, TypesData td, boolean dynamic, Tokenizer t) throws IOException {
        this(l, td, dynamic, loadSupercatRuleCombos(combosURL), t);       
        ruleScanner.parse(rulesUrl);
        this.td = null; //not strictly necessary and maybe even pedantic, but it's kind of a hack to use an instance variable like this.
    }
    private XmlScanner ruleScanner = new XmlScanner() {
    	public void handleElement(Element ruleEl) {
            String active = ruleEl.getAttributeValue("active");
            if (active == null || active.equals("true")) {
                try { rulesToAdd.add(readRule(ruleEl)); }
                catch (RuntimeException exc) {
                    System.err.println("Skipping rule: " + ruleEl.getAttributeValue("name"));
                    System.err.println(exc.toString());
                }
            }
    	}
    };
    
    // reads in a rulepredsToRules
    private RuleStruct readRule(Element ruleEl) {
        RuleStruct r;
        String type = ruleEl.getName();
        if (type.equals("application")) {
            String dir = ruleEl.getAttributeValue("dir");
            if (dir.equals("forward")) {
                r = new RuleStruct(RuleType.ForwardApplication, l, t);
            } else {
                r = new RuleStruct(RuleType.BackwardApplication, l, t);
            }
        } else if (type.equals("composition")) {
            String dir = ruleEl.getAttributeValue("dir");
            String harmonic = ruleEl.getAttributeValue("harmonic");
            boolean isHarmonic = new Boolean(harmonic).booleanValue();
            if (dir.equals("forward")) {
                r = new CompSubRuleStruct(RuleType.ForwardComposition, l, isHarmonic, t);
            } else {
                r = new CompSubRuleStruct(RuleType.BackwardComposition, l, isHarmonic, t);
            }
        } else if (type.equals("substitution")) {
            String dir = ruleEl.getAttributeValue("dir");
            String harmonic = ruleEl.getAttributeValue("harmonic");
            boolean isHarmonic = new Boolean(harmonic).booleanValue();
            if (dir.equals("forward")) {
                r = new CompSubRuleStruct(RuleType.ForwardSubstitution, l, isHarmonic, t);
            } else {
                r = new CompSubRuleStruct(RuleType.BackwardSubstitution, l, isHarmonic, t);
            }
        } else if (type.equals("typeraising")) {
            String dir = ruleEl.getAttributeValue("dir");
            String useDollar = ruleEl.getAttributeValue("useDollar");
            boolean addDollar = new Boolean(useDollar).booleanValue();
            Category arg = null;
            Element argElt = ruleEl.getChild("arg");
            if (argElt != null) {
                arg = CatReader.getCat(l, td, (Element)argElt.getChildren().get(0));
            }
            Category result = null;
            Element resultElt = ruleEl.getChild("result");
            if (resultElt != null) {
                result = CatReader.getCat(l, td, (Element)resultElt.getChildren().get(0));
            }
            if (dir.equals("forward")) {
                r = new TypeRaisingRuleStruct(RuleType.ForwardTypeRaising, l, addDollar, arg, result, t);
            } else {
                r = new TypeRaisingRuleStruct(RuleType.BackwardTypeRaising, l, addDollar, arg, result, t);
            }
        } else if (type.equals("typechanging")) {
            r = readTypeChangingRule(ruleEl);
        } else {
            throw new RuntimeException("Invalid element in rules: " + type);
        }
        return r;
    }
    
    // reads in a type changing rule
    private RuleStruct readTypeChangingRule(Element ruleEl) {      
        String rname = ruleEl.getAttributeValue("name");
        Element argCatElt = (Element)ruleEl.getChild("arg").getChildren().get(0);
        Category arg = CatReader.getCat(l, td, argCatElt);
        Element resultCatElt = (Element)ruleEl.getChild("result").getChildren().get(0);
        Element lfElt = resultCatElt.getChild("lf");
        Category result = CatReader.getCat(l, td, resultCatElt);
        LF firstEP = null;
        if (lfElt != null) {
            firstEP = HyloHelper.firstEP(l, td, HyloHelper.getLF(l, td, lfElt));
        }
        return new TypeChangingRuleStruct(l, arg, result, firstEP, rname, t);     
    }

   
    
//    /**
//     * Sets the dynamic combos flag to the given value, controlling whether the 
//     * observed supercat combos is determined dynamically.
//     */
//    public void setDynamicCombos(boolean dynamic) {
//    	
//    }
//    
   
    
    /** 
     * Loads the observed supercat-rule combos, for filtering. 
     * Only file URLs are supported at present.
     * Missing files are ignored. 
     **/
    private static Map<SupercatCombo, SupercatRuleCombo> loadSupercatRuleCombos(URL url) throws IOException {
    	Map<SupercatCombo, SupercatRuleCombo> supercatRuleCombos = new LinkedHashMap<SupercatCombo, SupercatRuleCombo>();
    	if (url == null) return supercatRuleCombos;
    	File combosFile = new File(url.getFile());
    	if (!combosFile.exists()) return supercatRuleCombos;
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
    	return supercatRuleCombos;
    }
    
  
    
 

    
}
