///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2002-5 Jason Baldridge and University of Edinburgh (Michael White)
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

package synsem;

import grammar.TypesData;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import lexicon.Lexicon;

import org.jdom.Element;

import unify.GUnifier;
import unify.MutableScript;
import unify.Substitution;
import unify.Unifiable;
import unify.UnifyControl;
import unify.UnifyFailure;
import unify.Variable;

/**
 * A category which contains an unordered set of categories.
 * 
 * @author Jason Baldridge
 * @author Michael White
 * @version $Revision: 1.11 $, $Date: 2009/12/21 02:15:44 $
 */
public final class SetArg implements Arg, Serializable {

	private static final long serialVersionUID = -7067480310511294657L;
	
	private ArgStack _args;
	
	@SuppressWarnings("unchecked")
	public SetArg(Lexicon l, TypesData td, Element el) {
		List<Element> info = el.getChildren();
		List<Arg> args = new ArrayList<Arg>();
		for (Iterator<Element> infoIt = info.iterator(); infoIt.hasNext();) {
			Slash s = new Slash(infoIt.next());
			Category c = CatReader.getCat(l, td, infoIt.next());
			args.add(new BasicArg(s, c));
		}
		Arg[] list = new Arg[args.size()];
		args.toArray(list);
		_args = new ArgStack(list);
	}

	public SetArg(Arg[] args) {
		_args = new ArgStack(args);
	}

	public SetArg(ArgStack args) {		
		_args = args;
	}

	public Arg copy() {
		return new SetArg(_args.copy());
	}

	public void add(ArgStack as) {
		_args.add(as);
	}

	public void applyToAll(CatScript fcn) {
		_args.applyToAll(fcn);
	}

	public Arg copyWithout(int pos) {
		if (_args.size() == 2) {
			if (pos == 0) {
				return _args.get(1);
			} else {
				return _args.get(0);
			}
		} else {
			return new SetArg(_args.copyWithout(pos));
		}
	}

	public int size() {
		return _args.size();
	}

	public BasicArg get(int pos) {
		return (BasicArg) _args.get(pos);
	}

	public Category getCat(int pos) {
		return ((BasicArg) _args.get(pos)).getCat();
	}

	public int indexOf(UnifyControl uc, BasicArg a) {
		int index = -1;
		for (int i = 0; i < _args.size() && index < 0; i++) {
			try {
				a.unifySlash(((BasicArg) _args.get(i)).getSlash());
				GUnifier.unify(uc, getCat(i), a.getCat());
				index = i;
			} catch (UnifyFailure uf) {
			}
		}
		return index;
	}

	public int indexOf(UnifyControl uc, Category cat) {
		int index = -1;
		for (int i = 0; i < _args.size() && index < 0; i++) {
			try {
				GUnifier.unify(uc, getCat(i), cat);
				index = i;
			} catch (UnifyFailure uf) {
			}
		}
		return index;
	}

	public void setSlashModifier(boolean modifier) { 
		for (int i = 0; i < _args.size(); i++) {
			BasicArg arg = get(i);
			arg.setSlashModifier(modifier);
		}
	}
	
    public void setSlashHarmonicCompositionResult(boolean harmonicResult) { 
		for (int i = 0; i < _args.size(); i++) {
			BasicArg arg = get(i);
			arg.setSlashHarmonicCompositionResult(harmonicResult);
		}
	}
    
	public boolean containsContrarySlash() {
		for (int i = 0; i < _args.size(); i++) {
			if (!((BasicArg) _args.get(i)).getSlash().sameDirAsModality()) {
				return true;
			}
		}
		return false;
	}

	public void unifySlash(Slash s) throws UnifyFailure {
		for (int i = 0; i < _args.size(); i++) {
			_args.get(i).unifySlash(s);
		}
	}

	public void unifyCheck(Object u) throws UnifyFailure {
	}

	// nb: direct unification not implemented ...
	public Object unify(Object u, Substitution sub, UnifyControl uc) throws UnifyFailure {
		throw new UnifyFailure();
	}

	public Object fill(UnifyControl uc, Substitution s) throws UnifyFailure {
		return new SetArg(_args.fill(uc, s));
	}

	public void mutateAll(MutableScript m) {
		_args.mutateAll(m);
	}

	public boolean occurs(Variable v) {
		return _args.occurs(v);
	}

	public boolean equals(Object c) {
		return false;
	}

	public String toString() {
		StringBuffer sb = new StringBuffer(10);
		sb.append('{').append(_args.toString()).append('}');
		return sb.toString();
	}

	/**
	 * Returns the supertag for this arg.
	 */
	public String getSupertag() {
		StringBuffer sb = new StringBuffer();
		sb.append("{").append(_args.getSupertag()).append("}");
		return sb.toString();
	}

	/**
	 * Returns a TeX-formatted string representation for this arg.
	 */
	public String toTeX() {
		StringBuffer sb = new StringBuffer(10);
		sb.append("\\{").append(_args.toTeX()).append("\\}");
		return sb.toString();
	}

	/**
	 * Returns a hash code for this arg, using the given map from vars to ints.
	 */
	public int hashCode(Map<Unifiable, Integer> varMap) {
		return _args.hashCode(varMap);
	}

	/**
	 * Returns whether this arg equals the given object up to variable names,
	 * using the given maps from vars to ints.
	 */
	public boolean equals(Object obj, Map<Unifiable, Integer> varMap, Map<Unifiable, Integer> varMap2) {
		if (obj.getClass() != this.getClass()) {
			return false;
		}
		SetArg sa = (SetArg) obj;
		return _args.equals(sa._args, varMap, varMap2);
	}
}
