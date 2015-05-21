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

import grammar.Types;
import grammar.TypesData;
import lexicon.Lexicon;

import org.jdom.Element;

import synsem.LF;
import unify.SimpleType;
import unify.Substitution;
import unify.UnifyControl;
import unify.UnifyFailure;

/**
 * A hybrid logic nominal, an atomic formula which holds true at exactly one
 * point in a model.
 * The type is checked for compatibility during unification with nominal vars, 
 * but it is not updated, since nominal atoms are constants.
 * If no type is given, the TOP type is used for backwards compatibility.
 *
 * @author      Jason Baldridge
 * @author      Michael White
 * @version     $Revision: 1.10 $, $Date: 2009/07/17 04:23:30 $
 **/
public class NominalAtom extends HyloAtom implements Nominal {

	private static final long serialVersionUID = -6002484920078196411L;
	
	protected boolean shared = false;
    
    public NominalAtom(Lexicon lex, TypesData td, String name) {
        this(lex, name, td.getSimpleType(Types.TOP_TYPE));
    }
    
    public NominalAtom(Lexicon lex, String name, SimpleType st) {
        this(lex, name, st, false);
    }
    
    public NominalAtom(Lexicon lex, String name, SimpleType st, boolean shared) {
        super(lex, name, st);
        type = st;
        this.shared = shared;
    }

    public String getName() { return _name; }
    
    public boolean isShared() { return shared; }

    public void setShared(boolean shared) { this.shared = shared; }
    
    public LF copy() {
        return new NominalAtom(l, _name, type, shared);
    }

    /** Returns a hash code based on the atom name and type. */
    public int hashCode() { 
        return _name.hashCode() + type.hashCode();
    }

    /**
     * Returns whether this atom equals the given object based on the atom name and type.
     */
    public boolean equals(Object obj) {
        if (!super.equals(obj)) return false;
        NominalAtom nom = (NominalAtom) obj;
        return type.equals(nom.type);
    }

    public Object unify(Object u, Substitution sub, UnifyControl uc) throws UnifyFailure {
        if (equals(u)) return this;
        return super.unify(u, sub, uc);
    }
    
    public int compareTo(Nominal nom) {
        if (nom instanceof NominalAtom) { 
            return super.compareTo((NominalAtom)nom);
        }
        int retval = _name.compareTo(nom.getName());
        if (retval == 0) { retval = -1; } // atom precedes var if names equal
        return retval;
    }
    
    public String toString() {
        String retval = _name;
        if (!type.getName().equals(Types.TOP_TYPE)) retval += ":" + type.getName();
        return retval;
    }
    
    /**
     * Returns an XML representation of this LF.
     */
    public Element toXml() {
        Element retval = new Element("nom");
        retval.setAttribute("name", toString());
        return retval;
    }

	@Override
	public String prettyPrint(String indent) {
		return prettyPrint(indent);
	}
}
