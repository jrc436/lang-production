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

import hylo.HyloHelper;

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import synsem.AtomCat;
import synsem.Category;
import synsem.LF;
import unify.FeatureStructure;
import unify.Mutable;
import util.GroupMap;

/**
 * Adds the features from macros to a category.
 *
 * @author      Jason Baldridge
 * @author      Gann Bierner
 * @author      Michael White
 * @version     $Revision: 1.6 $, $Date: 2011/03/20 20:11:57 $
 */
public class MacroAdder {

    private GroupMap<Integer, FeatureStructure> _specificMacros;
    private List<MacroItem> _macroItems; // for LF macros
    private final Lexicon l;
    
    public MacroAdder(GroupMap<Integer, FeatureStructure> sm, List<MacroItem> macroItems, Lexicon l) {
        _specificMacros = sm;
        _macroItems = macroItems;
        this.l = l;
    }

    public void addMacros(Category cat) {
        // add features 
        cat.mutateAll(this::addIndexedFeatures);
        // append preds to LF
        LF lf = cat.getLF();
        for (int i=0; i < _macroItems.size(); i++) {
            MacroItem mi = _macroItems.get(i);
            LF[] preds = mi.getPreds();
            for (int j=0; j < preds.length; j++) {
                LF pred = (LF) preds[j].copy();
                if (!HyloHelper.isElementaryPredication(pred)) {
                    System.err.println(
                        "Warning: ignoring LF macro pred, which is not an elementary predication: " +
                        pred
                    );
                    continue;
                }
                lf = HyloHelper.append(l, lf, pred);
            }
        }
        // sort and reset LF
        HyloHelper.sort(l, lf);
        cat.setLF(lf);
    }
    
    private void addIndexedFeatures(Mutable c) {
            if (c instanceof AtomCat) {
                FeatureStructure fs = ((AtomCat)c).getFeatureStructure();
                int fsIndex = fs.getIndex();
                Set<FeatureStructure> featStrucs = _specificMacros.get(fsIndex);
                if (null == featStrucs) {
                    return;
                }
                FeatureStructure $fs = fs.copy();
                for (Iterator<FeatureStructure> fsIt = featStrucs.iterator(); fsIt.hasNext();) {
                    FeatureStructure macroFS = (FeatureStructure) fsIt.next();
                    $fs = $fs.inherit(macroFS);
                }
                ((AtomCat)c).setFeatureStructure($fs);
          }
    }
}
