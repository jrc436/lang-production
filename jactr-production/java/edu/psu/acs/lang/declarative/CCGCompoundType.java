package edu.psu.acs.lang.declarative;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

/**
 * A recursively implmemented data structure to represent a CCG Type. Every CCG Type decomposes into operators (slash, backslash) 
 * and basetypes, possibly with modifiers. Note that in this class "left" is only referring to its placement, not the CCG-idea of its placement.
 * For instance, an S\NP requires an NP to the right, but "S" would be the left slot, and NP would be the right slot, backslash the operator
 * 
 * To actually extract dependencies, you would look at the highest level. At every recursive level, the dependency is pretty straightforward.
 * If leftCCGType requires rightCCGType on its left (equivalently, right CCGType requires leftCCGType on its right): connector = backslash
 * if left requires right on its right (or right requires left on its left): connector = slash
 * @author jrc
 *
 */

//note after an application, more or less, the type "shrinks" as its dependency is dropped. It then recurses, taking the right and left of: 
//1. Forward Application (Slash), The right CCGtype is resolved and disappears. The CCG then recurses to the leftCCGtype
//2. Backward Application (Backslash), The left CCGtype is resolved and dissappears. The CCG term then recurses to the right CCG type
//3. Forward Composition (Slash), The right CCG type on type 1 is resolved, while the left CCG type on type 2 is resolved, and they are combined.
//   This recurses in such a way that left->leftCCGtype (type1) and right-> rightCCGtype (type2). Connector stays slash., making right the new open
//4. Backward Composition (Backslash), The left CCG type on type 1 is resolved along with the right CCG type on type 2, and they are combined
//   In other words, left->leftCCGtype (type2) and right -> rightCCGtype(type1)
public class CCGCompoundType extends CCGType {
	private CCGType leftCCGType; //recursive case (always null in base)
	private CCGType rightCCGType; //recursive case (always null in base)
	private CCGOperator connector; //the slash or backslash connecting the left or right (always null in base)
	private ConjEnum conjable; //starts as null until initializaiton is complete
	public String toString() {
		String left = leftCCGType instanceof CCGCompoundType ? "(" + leftCCGType.toString() + ")" : leftCCGType.toString();
		String right = rightCCGType instanceof CCGCompoundType ? "(" + rightCCGType.toString() + ")" : rightCCGType.toString();
		return left + connector.toString() + right;
	}
	/**
	 * The string of the expanded type. It will take it and recursively decompose it into a tree (more or less)
	 * @param s
	 */
	public Set<CCGType> harvestTypes() {
		Set<CCGType> toRet = new HashSet<CCGType>();
		toRet.add(this);
		Stack<CCGType> traverse = new Stack<CCGType>();
		traverse.add(this);
		while (!traverse.isEmpty()) {
			CCGType popped = traverse.pop();
			toRet.add(popped);
			if (popped instanceof CCGCompoundType) {
				CCGCompoundType pop = (CCGCompoundType) popped;
				traverse.add(pop.leftCCGType);
				traverse.add(pop.rightCCGType);
			}
		}
		return toRet;
	}
	public static CCGCompoundType makeType(CCGType left, CCGType right, CCGOperator connector) {
		CCGCompoundType cg = new CCGCompoundType(left, right, connector);
		cg.addSlot(CCGTypeSlot.FullType, cg);
		return cg;
	}
	private CCGCompoundType(CCGType left, CCGType right, CCGOperator connector) {
		super(left, right, connector);
		this.leftCCGType = left;
		this.rightCCGType = right;
		this.connector = connector;
	}
	public CCGOperator getCombo() {
		return connector;
	}
	public CCGType getLeft() {
		return leftCCGType;
	}
	public CCGType getRight() {
		return rightCCGType;
	}
	
	public static CCGType makeCCGType(String s, boolean alternateSeperators) {
		//System.out.println("Starting on string:"+s);
		CCGType parent = alternateSeperators ? recCreateTypes(s, new CCGOperator(CCGOperatorEnum.Slash).toString().charAt(0), new CCGOperator(CCGOperatorEnum.Backslash).toString().charAt(0))
											 : recCreateTypes(s, '/', '\\');
		 //right now we know the conjable is null, and we know that only its right most type could possibly have conj set to true
		 if (parent.isConjable()) {
			 parent.purifyConj();
			 parent.makeConjable();
		 }
		 else {
			 parent.makeUnconjable();
		 }
		 return parent;
	}
	private static CCGType recCreateTypes(String s, char fappSep, char bappSep) {
		int parenStack = 0;
		int paren = s.indexOf('(');
		int slash = s.indexOf(fappSep);
		int bslash = s.indexOf(bappSep);
		//this means we're in a base type. This is the base case.
		if (slash == -1 && bslash == -1) {
			CCGTypeModifier modi = null;
			if (s.contains("[")) {
				int openb = s.indexOf("[");
				int closeb = s.indexOf("]");
				if (closeb == -1) {
					System.err.println("Type seems to contain open bracket but not close bracket??");
					System.err.print(s);
					System.exit(1);
				}
				String mod = s.substring(openb+1, closeb);
				//System.out.println("mod:"+mod);
				modi = CCGTypeModifier.value(mod);
				s = s.substring(0, openb);
			}
			//System.out.println("basetype:"+s);
			return CCGBaseType.makeType(CCGBaseTypeEnum.value(s), modi);
		}
		//this means we have an exposed dependency with a backslash!
		if (bslash != -1 && (bslash < slash || slash == -1) && (bslash < paren || paren == -1)) {
			slash = -1;
		}
		//exposed dependency with a slash!
		else if (slash != -1 && (slash < bslash || bslash == -1) && (slash < paren || paren == -1)) {
			bslash = -1;
		}
		//uh oh, complex type on the left side... we'll have to first find where it ends.
		else if ((paren < slash || slash == -1) && (paren < bslash || bslash == -1) && paren != -1) {
			//it should always be the first character:
			if (paren != 0) {
				System.err.println("Parenthesis is not the first character in a complex type being mined");
				System.err.println(paren);
				System.err.println(s);
				System.exit(1);
			}
			//if this is true, we need to read through the parenstack and get to the slash or backslash exposed
			for (int i = paren; i < s.length(); i++) {
				if (s.charAt(i) == '(') {
					parenStack++;
				}
				else if (s.charAt(i) == ')') {
					parenStack--;
				}
				if (parenStack == 0) {
					if (s.charAt(i+1) == fappSep) {
						slash = i+1;
						bslash = -1;
						break; //ok, we're out of it!
					}
					else if (s.charAt(i+1) == bappSep) {
						bslash = i+1;
						slash = -1;
						break;
					}
					else {
						System.err.println("The parenstack is over, but there is no combinator? Excess parens surrounding type would cause this");
						System.err.println(s);
						System.err.println(i+1);
						System.exit(1);
					}
				}
				else if (parenStack < 0) {
					System.err.println("Somehow, we dug deeper than the parenstack. Malformed input.");
					System.err.println(s);
					System.err.println(s.charAt(i));
					System.exit(1);
				}
			}
		}
		else {
			//just in case...
			System.err.println("Some weird combination of bullshit must have occurred.");
			System.err.println(s);
			System.err.println("Slash index:"+slash);
			System.err.println("Backslash index:"+bslash);
			System.err.println("Paren index:"+paren);
			System.exit(1);
		}
		//so we have left or right set now...
		String left = "";
		String right = "";
		CCGOperatorEnum c = null;
		if (slash > 0) {
			left = s.substring(0, slash);
			right = s.substring(slash+1, s.length());
			c = CCGOperatorEnum.Slash;
		}
		else if (bslash > 0){
			left = s.substring(0, bslash);
			right = s.substring(bslash+1, s.length());
			c = CCGOperatorEnum.Backslash;
		}		
		else {
			System.err.println("Backslash is somehow zero, must be an error");
			System.err.println(s);
			System.exit(1);
		}
		if (left.length() == 0 || right.length() == 0) {
			System.err.println(left);
			System.err.println(right);
			System.err.println(s);
		}
		left = stripParens(left);
		right = stripParens(right);
		return makeType(recCreateTypes(left, fappSep, bappSep), recCreateTypes(right, fappSep, bappSep), new CCGOperator(c));
	}
	private static String stripParens(String s) {
		if (s.charAt(0) == '(') {
			//ok, it is a complex type still, so we will have to strip it.
			if (s.charAt(s.length()-1) != ')') {
				System.err.println("Mismatched parentheses. Most likely did something wrong.");
				System.err.println(s);
				System.err.println(s);
				System.exit(1);
			}
			s = s.substring(1, s.length()-1);
			//it's okay if there are more parentheses, it just means there are some really complex types goin on
		} //otherwise it's not type-reducible, and must be a base type
		return s;
	}
	//isConjable, after the creation of the type is complete, should always have conjable set. Before conjable is set, for purposes of convenient recursion, we will use
	//the initial setting, where isConjable refers to whether it is or will eventually be conjable. 
	@Override
	public boolean isConjable() {
		//return leftCCGType.isConjable() || rightCCGType.isConjable();
		return conjable == null ? checkConjability() : conjable == ConjEnum.Conjable;
	}
	//initially, the most right type will be the determiner for whether or not its conjable
	private boolean checkConjability() {
		return rightCCGType.isConjable();
	}
	@Override
	protected void purifyConj() {
		rightCCGType.purifyConj();
	}
	@Override
	protected void makeConjable() {
		conjable = ConjEnum.Conjable;	
	}
	@Override
	protected void makeUnconjable() {
		conjable = ConjEnum.Nonconjable;
	}
}
