///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003 Jason Baldridge, Gann Bierner and 
//                    University of Edinburgh (Michael White)
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

import grammar.Grammar;
import hylo.HyloHelper;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import synsem.AtomCat;
import synsem.Category;
import synsem.LF;
import unify.FeatureStructure;
import unify.ModFcn;
import unify.Mutable;
import util.IntHashSetMap;

/**
 * Adds the features from macros to a category.
 *
 * @author      Jason Baldridge
 * @author      Gann Bierner
 * @author      Michael White
 * @version     $Revision: 1.6 $, $Date: 2011/03/20 20:11:57 $
 */
public class MacroAdder {

    private IntHashSetMap _specificMacros;
    private List<MacroItem> _macroItems; // for LF macros
    private final Grammar grammar;
    
    public MacroAdder(IntHashSetMap sm, List<MacroItem> macroItems, Grammar grammar) {
    	if (grammar == null ) {
    		System.err.println("Someone's tricksing you");
    		System.exit(1);
    	}
        _specificMacros = sm;
        _macroItems = macroItems;
        this.grammar = grammar;
    }

    public void addMacros(Category cat) {
        // add features 
        cat.deepMap(addIndexedFeatures);
        // append preds to LF
        LF lf = cat.getLF();
        for (int i=0; i < _macroItems.size(); i++) {
            MacroItem mi = _macroItems.get(i);
            LF[] preds = mi.getPreds();
            for (int j=0; j < preds.length; j++) {
                LF pred = (LF) preds[j].copy();
                if (!HyloHelper.isElementaryPredication(pred)) {
                    System.out.println(
                        "Warning: ignoring LF macro pred, which is not an elementary predication: " +
                        pred
                    );
                    continue;
                }
                lf = HyloHelper.append(grammar, lf, pred);
            }
        }
        // sort and reset LF
        HyloHelper.sort(grammar, lf);
        cat.setLF(lf);
    }
    
    private ModFcn addIndexedFeatures = new ModFcn() {
        @SuppressWarnings("rawtypes")
		public void modify(Mutable c) {
            if (c instanceof AtomCat) {
                FeatureStructure fs = ((AtomCat)c).getFeatureStructure();
                int fsIndex = fs.getIndex();
                Set featStrucs = (Set)_specificMacros.get(fsIndex);
                if (null == featStrucs) {
                    return;
                }
                FeatureStructure $fs = fs.copy();
                for (Iterator fsIt = featStrucs.iterator(); fsIt.hasNext();) {
                    FeatureStructure macroFS = (FeatureStructure) fsIt.next();
                    $fs = $fs.inherit(macroFS);
                }
                ((AtomCat)c).setFeatureStructure($fs);
            }
        }
    };
}
