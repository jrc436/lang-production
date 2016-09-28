package edu.psu.acs.lang.util;

import edu.psu.acs.lang.declarative.CCGCompoundType;
import edu.psu.acs.lang.declarative.CCGType;
import edu.psu.acs.lang.production.SyntaxRuleType;

public class RuleNode implements ParseNode {
	private CCGType result;
	private ParseNode left;
	private ParseNode right;
	private SyntaxRuleType rule;
	public RuleNode(CCGType result, ParseNode left, ParseNode right, SyntaxRuleType rule) throws ParseException {	
		if (result == null || left == null || rule == null) {
			System.err.println(left);
		    System.err.println(right);
			System.err.println(rule);
			System.err.println(result);
			throw new ParseException("Null values for result, left, and rule are never allowed");
		}
		if (rule.isTypeRaise() && right != null) {
			if (left.getType().isPCT()) {
				left = right;
				right = null;
			}
			else if (right.getType().isPCT()) {
				right = null;
			}
			//it could also be a conj, in which case we have to do some bullshit fk magic because they are likely cheating.
			else if (left.getType().isConjugation()) {
				//so... it's a typeraise and they're combining it with a conjugation because who cares, right?
				//so we need to return a rulenode that has an embedded rulenode, gross.
//				if (right instanceof RuleNode) {
//					//so do the conj, then the TCR..? but add the conj to the TCR typ
//					//so the result of the conj doesn't technically matter...
					CCGCompoundType.reverseConjability(right.getType());
					left = new RuleNode(right.getType(), left, right, SyntaxRuleType.CONJ);
					right = null;
				//}
			}
			else if (right.getType().isConjugation()) {
				CCGCompoundType.reverseConjability(left.getType());
				right = new RuleNode(left.getType(), left, right, SyntaxRuleType.CONJ);
				left = null;
			}
			else {
				System.err.println(left);
			    System.err.println(right);
				System.err.println(rule);
				System.err.println(result);
				throw new ParseException("TypeRaise should only have one node!");
			}	
		}
//		
//		//typeraise should only have one node
//		if ((rule == SyntaxRuleType.TypeRaise || rule == SyntaxRuleType.TPC) && right != null) {
//		
//		}
//		if (rule == SyntaxRuleType.TCR && right != null && !left.getType().isPCT() && !right.getType().isPCT() && !left.getType().isConjugation() && !right.getType().isConjugation()) {
//			System.err.println(left);
//		    System.err.println(right);
//			System.err.println(rule);
//			System.err.println(result);
//			throw new ParseException("I'm not really sure wtf TCR is, but it might be wrong?");
//		}
		//not typeraise should have exactly two nodes
		if (!(rule == SyntaxRuleType.TypeRaise || rule == SyntaxRuleType.TCR || rule == SyntaxRuleType.TPC) && right == null) {
			System.err.println(left);
		    System.err.println(right);
			System.err.println(rule);
			System.err.println(result);
			throw new ParseException("Only TypeRaise should only have one node!");
		}
		this.left = left;
		this.right = right;
		this.rule = rule;
		this.result = result;
	}
	
	public SyntaxRuleType getRule() {
		return rule;
	}
	@Override
	public String toString() {
		String rightString = right == null ? " " : " "+right.toString()+" ";
		String toReturn = "{";
		toReturn+=result.toString()+" ";
		toReturn+="(comb:"+rule.toString()+") ";
		toReturn+=left.toString()+rightString;
		toReturn+="}";
		return toReturn;
	}
	public String getPhrase() {
		String rightPhrase = right == null ? " " : " "+right.getPhrase()+" ";
		return (left.getPhrase() + rightPhrase).trim();
	}
	public ParseNode getLeftChild() {
		return left;
	}
	public ParseNode getRightChild() {
		return right;
	}
	public void setLeftChild(ParseNode pn) {
		this.left = pn;
	}
	public void setRightChild(ParseNode pn) {
		this.right = pn;
	}
	public String typeToString() {
		return result.toString();
	}
	public CCGType getType() {
		return result;
	}
	
	public boolean validate(String leftType, String rightType, String lword, String rword, SyntaxRuleType type) {
		return type.equals(rule) && leftType.equals(left.typeToString()) && rightType.equals(right.typeToString()) && lword.equals(left.getPhrase()) && rword.equals(right.getPhrase());
	}
	@Override
	public boolean validate(Object[] validators) {
		if (validators.length != 5) {
			throw new IllegalArgumentException("RuleNode has 5 validators, 1: lefttype, 2: righttype, 3: leftword, 4:rightword, 5:ruletype");
		}
		//ideally we would validate... but right now, we have no way of combining strings.
		if (!(validators[0] instanceof String) || !(validators[1] instanceof String) || /*!(validators[2] instanceof String) || !(validators[3] instanceof String) ||*/ !(validators[4] instanceof SyntaxRuleType)) {
			throw new IllegalArgumentException("The First Four of the validators are Strings, the final one is a SyntaxRuleType");
		}
		return left.typeToString().equals(validators[0]) && (right == null || right.typeToString().toString().equals(validators[1])) && /*left.getPhrase().equals(validators[2]) && right.getPhrase().equals(validators[3]) &&*/ rule.equals(validators[4]);
	}
	
	@Override
	public boolean equals(Object o) {
		if (o == null || !o.getClass().equals(this.getClass())) {
			return false;
		}
		RuleNode other = (RuleNode) o;
		return other.left.equals(this.left) && other.right.equals(this.right) && other.rule.equals(this.rule) && other.result.equals(this.result);
	}
	@Override
	public int hashCode() {
		return this.left.hashCode() * 13 + this.right.hashCode() * (int)Math.pow(13, 2) + this.rule.hashCode() * (int)Math.pow(13, 3) + this.result.hashCode() * (int)Math.pow(13, 4);
	}
}
