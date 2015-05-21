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

package hylo;

import grammar.TypesData;
import lexicon.Lexicon;

import org.jdom.Element;

import synsem.LF;
import unify.Substitution;
import unify.Unifier;
import unify.UnifyControl;
import unify.UnifyFailure;

/**
 * A modal diamond operator, such as &lt;P&gt;p.
 *
 * @author      Jason Baldridge
 * @author      Michael White
 * @version     $Revision: 1.6 $, $Date: 2009/07/17 04:23:30 $
 **/
public final class Diamond extends ModalOp {

	private static final long serialVersionUID = 543211908001651361L;

	public Diamond(Lexicon l, TypesData td, Element e) {
        super(l, td, e);
    }

    public Diamond(Lexicon l, Mode mode, LF arg) {
        super(l, mode, arg);
    }

    public LF copy() {
        return new Diamond(l, (Mode)_mode.copy(), _arg.copy());
    }
    
    public boolean equals(Object o) {
        if (o instanceof Diamond) {
            return super.equals((Diamond)o);
        } else {
            return false;
        }
    }

    public void unifyCheck(Object u) throws UnifyFailure {
        if (u instanceof Diamond) {
            super.unifyCheck((Diamond)u);
        } else {
            throw new UnifyFailure();
        }
    }

    public Object unify(Object u, Substitution sub, UnifyControl uc) throws UnifyFailure {
        if (u instanceof HyloFormula) {
            if (u instanceof Diamond) {
                Mode $mode = (Mode) Unifier.unify(uc, _mode, ((Diamond)u)._mode, sub);
                LF $arg = (LF) Unifier.unify(uc, _arg, ((Diamond)u)._arg,sub);
                return new Diamond(l, $mode, $arg);
            }
            else return super.unify(u, sub, uc);
        } else {
            throw new UnifyFailure();
        }
    }

    public Object fill(UnifyControl uc, Substitution sub) throws UnifyFailure {
        return new Diamond(l, (Mode)_mode.fill(uc, sub), (LF)_arg.fill(uc, sub));
    }
    
    /** Returns the string form of this modal op, without the arg. */
    public String modalOpString() {
        return new StringBuffer().append('<').append(_mode.toString()).append('>').toString();
    }
    
    /**
     * Returns an XML representation of this LF.
     */
    public Element toXml() {
        Element retval = new Element("diamond");
        retval.setAttribute("mode", _mode.toString());
        Element argElt = _arg.toXml();
        retval.addContent(argElt);
        return retval;
    }

	@Override
	public String prettyPrint(String indent) {
		return prettyPrint(indent);
	}
}
