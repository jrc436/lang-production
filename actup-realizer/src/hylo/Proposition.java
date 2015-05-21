///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-4 Jason Baldridge and University of Edinburgh (Michael White)
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

package hylo;

import lexicon.Lexicon;

import org.jdom.Element;

import synsem.LF;
import unify.SimpleType;
import unify.Substitution;
import unify.UnifyControl;
import unify.UnifyFailure;

/**
 * A propositional value, such as the predicate "sleep", 
 * or the value of a semantic feature, such as "past" for tense.
 * Types are unified if present.
 *
 * @author      Jason Baldridge
 * @author      Michael White
 * @version     $Revision: 1.7 $, $Date: 2009/07/17 04:23:30 $
 **/
public class Proposition extends HyloAtom {

	private static final long serialVersionUID = -5392519210634765414L;

	public Proposition(Lexicon lex, String name) {
        super(lex, name);
    }
    
    public Proposition(Lexicon l, String name, SimpleType st) {
        super(l, name, st);
    }
    
    public LF copy() {
        return new Proposition(l, _name, type);
    }
    
    public Object unify(Object u, Substitution sub, UnifyControl uc) throws UnifyFailure {
        // check equality
        if (equals(u)) return this;
        // check for prop with compatible type
        if (u instanceof Proposition) {
            Proposition prop = (Proposition) u;
            if (type == null || prop.type == null) throw new UnifyFailure();
            SimpleType st = (SimpleType) type.unify(prop.type, sub, uc);
            // return prop with most specific type
            if (st.equals(type)) return this;
            if (st.equals(prop.type)) return prop;
            // otherwise return prop with name of intersection type
            return new Proposition(l, st.getName(), st);
        }
        // otherwise defer to default routine
        return super.unify(u, sub, uc);
    }
    
    /**
     * Returns an XML representation of this LF.
     */
    public Element toXml() {
        Element retval = new Element("prop");
        retval.setAttribute("name", toString());
        return retval;
    }

	@Override
	public String prettyPrint(String indent) {
		return prettyPrint(indent);
	}
}
