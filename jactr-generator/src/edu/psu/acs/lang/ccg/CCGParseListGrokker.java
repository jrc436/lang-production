package edu.psu.acs.lang.ccg;

import java.io.File;
import java.util.Stack;

import edu.psu.acs.lang.production.SyntaxRuleType;
import edu.psu.acs.lang.util.LexNode;
import edu.psu.acs.lang.util.ParseException;
import edu.psu.acs.lang.util.ParseNode;
import edu.psu.acs.lang.util.RuleNode;
import util.sys.FileProcessor;

public class CCGParseListGrokker extends FileProcessor<CCGParseList, CCGParseList> {

	@Override
	public int getNumFixedArgs() {
		return 0;
	}

	@Override
	public boolean hasNArgs() {
		return false;
	}

	@Override
	public String getConstructionErrorMsg() {
		return "No arguments are required.";
	}

	@Override
	public CCGParseList getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		while (true) {
			try {
				return new CCGParseList(f.toPath(), true);
			} catch (ParseException e) {
				e.printStackTrace();
				f = super.getNextFile();
				if (f == null) {
					return null;
				}
			}
		}
	}

	@Override
	public void map(CCGParseList newData, CCGParseList threadAggregate) {
		for (ParseNode pn : newData.getParser().getTops()) {
			boolean add = true;
			Stack<ParseNode> s = new Stack<ParseNode>();
			s.push(pn);
			while (!s.empty()) {
				ParseNode cur = s.pop();
				if (cur instanceof RuleNode) {
					RuleNode rcur = (RuleNode) cur;
					ParseNode readyL = grokTypeRaise(grokPCT(rcur.getLeftChild()));
					ParseNode readyR = grokTypeRaise(grokPCT(rcur.getRightChild()));
					if (readyL == null || readyR == null) {
						add = false;
						break;
					}		
					rcur.setLeftChild(readyL);
					rcur.setRightChild(readyR);
					s.push(rcur.getLeftChild());
					s.push(rcur.getRightChild());				
				}
			}
			if (add) {
				threadAggregate.getParser().addTop(pn);
			}
		}
	}
	private ParseNode grokPCT(ParseNode pn) {
		if (!(pn instanceof RuleNode) || pn == null) {
			return pn;
		}
		RuleNode rn = (RuleNode) pn;
		if (rn.getRule() == SyntaxRuleType.PCT || rn.getRule() == SyntaxRuleType.UNK) {
			if (rn.getLeftChild().getType().isPCT()) {
				return rn.getRightChild();
			}
			else if (rn.getRightChild().getType().isPCT()) {
				return rn.getLeftChild();
			}
			else {
				System.err.println("One child should be PCT god damnit!");
				//System.exit(1);
				return null;
			}
		}
		return rn;
	}
	private ParseNode grokTypeRaise(ParseNode pn) {
		if (!(pn instanceof RuleNode) || pn == null) {
			return pn;
		}
		RuleNode rn = (RuleNode) pn;
		if (rn.getRule() == SyntaxRuleType.TypeRaise || rn.getRule() == SyntaxRuleType.TCR || rn.getRule() == SyntaxRuleType.TPC) {
			//System.out.println("Grokking type raise");
			if (rn.getLeftChild() instanceof RuleNode) {
				System.err.println("Unsupported Type Raise Detected");
				return null;
			}
			return new LexNode(rn.getPhrase(), rn.getType());
		}
		return rn;
	}
	@Override
	public void reduce(CCGParseList threadAggregate) {
		// TODO Auto-generated method stub
		
	}

}
