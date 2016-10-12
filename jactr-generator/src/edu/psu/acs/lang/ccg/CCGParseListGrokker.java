package edu.psu.acs.lang.ccg;

import java.io.File;
import java.util.Stack;

import edu.psu.acs.lang.declarative.type.CCGBaseType;
import edu.psu.acs.lang.declarative.type.CCGBaseTypeEnum;
import edu.psu.acs.lang.parsing.LexNode;
import edu.psu.acs.lang.parsing.ParseException;
import edu.psu.acs.lang.parsing.ParseNode;
import edu.psu.acs.lang.parsing.RuleNode;
import edu.psu.acs.lang.parsing.UnsupportedNode;
import edu.psu.acs.lang.production.SyntaxRuleType;
import util.sys.FileProcessor;

public class CCGParseListGrokker extends FileProcessor<CCGParseList, CCGParseList> {
	public CCGParseListGrokker() {
		super();
	}
	public CCGParseListGrokker(String inpDir, String outDir) {
		super(inpDir, outDir, new CCGParseList());
	}
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
				return new CCGParseList(f.toPath(), false);
			} catch (ParseException e) {
				e.printStackTrace();
				System.exit(1);
//				f = super.getNextFile();
//				if (f == null) {
//					return null;
//				}
			}
		}
	}

	@Override
	public void map(CCGParseList newData, CCGParseList threadAggregate) {
		for (ParseNode pn : newData.getParser().getTops()) {
			if (grokTop(pn, this::grokPCT) && grokTop(pn, this::grokTypeRaise)) {
				//System.err.println("Adding Top");
				threadAggregate.getParser().addTop(pn);
			}
		}
	}
	private boolean grokTop(ParseNode pn, Grokker grok) {
		boolean add = true;
		Stack<ParseNode> s = new Stack<ParseNode>();
		s.push(pn);
		while (!s.empty()) {
			ParseNode cur = s.pop();
			if (cur instanceof RuleNode) {
				RuleNode rcur = (RuleNode) cur;
				ParseNode readyL = grok.grok(rcur.getLeftChild());
				ParseNode readyR = grok.grok(rcur.getRightChild());
				if (readyL instanceof UnsupportedNode || readyR instanceof UnsupportedNode) {
					add = false;
					break;
				}
				rcur.setLeftChild(readyL);
				rcur.setRightChild(readyR);
				s.push(rcur.getLeftChild());
				s.push(rcur.getRightChild());				
			}
		}
		return add;
	}
	private ParseNode grokPCT(ParseNode pn) {
		if (!(pn instanceof RuleNode) || pn == null) {
			return pn;
		}
		RuleNode rn = (RuleNode) pn;
		if (rn.getRule() == SyntaxRuleType.PCT || rn.getRule() == SyntaxRuleType.UNK) {
			//System.out.println("Grokking PCT");
			if (rn.getLeftChild().getType().isPCT()) {
				return rn.getRightChild();
			}
			else if (rn.getRightChild().getType().isPCT()) {
				return rn.getLeftChild();
			}
			else {
				System.err.println("One child should be PCT god damnit!");
				//System.exit(1);
				return new UnsupportedNode();
			}
		}
		return rn;
	}
	private ParseNode grokTypeRaise(ParseNode pn) {
		if (!(pn instanceof RuleNode) || pn == null) {
			return pn;
		}
		RuleNode rn = (RuleNode) pn;
		if (rn.getRule().isTypeRaise()) {
			//System.out.println("Grokking type raise");
			if (rn.getLeftChild() instanceof RuleNode && !rn.getType().equals(CCGBaseType.makeType(CCGBaseTypeEnum.TOP, null))) {
				//System.err.println("Unsupported Type Raise Detected:");
				//System.err.println(rn.getLeftChild());
				return new UnsupportedNode();
			}
			return new LexNode(rn.getPhrase(), rn.getType());
		}
		return rn;
	}
	@Override
	public void reduce(CCGParseList threadAggregate) {
		synchronized(processAggregate) {
			for (ParseNode pn : threadAggregate.getParser().getTops()) {
				processAggregate.getParser().addTop(pn);
			}
		}
	}

}
