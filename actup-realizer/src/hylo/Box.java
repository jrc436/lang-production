///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2002 Jason Baldridge
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
import unify.UnifyControl;
import unify.UnifyFailure;


/**
 * A modal box operator, such as [F]q.
 *
 * @author      Jason Baldridge
 * @version     $Revision: 1.5 $, $Date: 2009/07/17 04:23:30 $
 **/
public final class Box extends ModalOp {

	private static final long serialVersionUID = 1575311851235814524L;

	public Box(Lexicon l, TypesData td, Element e) {
        super(l, td, e);
    }

    private Box(Lexicon l, Mode mode, LF arg) {
        super(l, mode, arg);
    }

    public LF copy() {
        return new Box(l, (Mode)_mode.copy(), _arg.copy());
    }
    
    public boolean equals(Object o) {
        if (o instanceof Box) {
            return super.equals((Box)o);
        } else {
            return false;
        }
    }

    public void unifyCheck(Object u) throws UnifyFailure {
        if (u instanceof Box) {
            super.unifyCheck((Box)u);
        } else {
            throw new UnifyFailure();
        }
    }

    public Object fill(UnifyControl uc, Substitution sub) throws UnifyFailure {
        return new Box(l, (Mode)_mode.fill(uc, sub), (LF)_arg.fill(uc, sub));
    }
    
    /** Returns the string form of this modal op, without the arg. */
    public String modalOpString() {
        return new StringBuffer().append('[').append(_mode.toString()).append(']').toString();
    }
    

    /**
     * Returns an XML representation of this LF (not currently supported).
     * Throws a runtime exception.
     */
    public Element toXml() {
        throw new RuntimeException("toXml() not currently supported for Box.");
    }

	@Override
	public String prettyPrint(String indent) {
		return prettyPrint(indent);
	}
}
