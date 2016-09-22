package edu.psu.acs.lang.lexsyn;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;

import edu.psu.acs.lang.declarative.CCGType;
import edu.psu.acs.lang.production.SyntaxRuleType;
import edu.psu.acs.lang.util.LexNode;
import edu.psu.acs.lang.util.ParseException;
import edu.psu.acs.lang.util.ParseNode;
import edu.psu.acs.lang.util.RuleNode;
import util.sys.FileProcessor;
import util.wordmap.CombineException;
import util.wordmap.WordMap;

public class WordInfoProcessor extends FileProcessor<CCGParseList, WordMap> {
	public WordInfoProcessor(String inpDir, String outDir, String[] bool) {
		super(inpDir, outDir, new WordMap(bool));
	}
	public WordInfoProcessor() {
		super();
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
		return "WordInfoProcessor requires no futher arguments";
	}

	@Override
	public CCGParseList getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		try {
			return new CCGParseList(Files.readAllLines(f.toPath()), true);
		} catch (ParseException | IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}

	@Override
	public void map(CCGParseList newData, WordMap threadAggregate) {
		Map<CCGType, Set<CCGType>> equivalences = new HashMap<CCGType, Set<CCGType>>();
		for (ParseNode n : newData.getTops()) {
			Stack<ParseNode> s = new Stack<ParseNode>();
			s.push(n);
			while (!s.empty()) {
				ParseNode cur = s.pop();
				if (cur instanceof RuleNode) {
					RuleNode rcur = (RuleNode) cur;
					rcur.setLeftChild(grokPCT(grokTypeRaise(rcur.getLeftChild(), equivalences)));
					rcur.setRightChild(grokPCT(grokTypeRaise(rcur.getRightChild(), equivalences)));
					s.push(rcur.getLeftChild());
					s.push(rcur.getRightChild());
				}
			}
		}
		
		HashMap<String, Set<CCGType>> map = new HashMap<String, Set<CCGType>>();
		List<ParseNode> parse = newData.getTops();
		List<LexNode> collect = new ArrayList<LexNode>();
		while (!parse.isEmpty()) {
			ParseNode pn = parse.remove(0);
			if (pn instanceof RuleNode) {
				RuleNode rn = (RuleNode) pn;
				parse.add(rn.getLeftChild());
				parse.add(rn.getRightChild());
			}
			else if (pn instanceof LexNode) {
				collect.add((LexNode)pn);
			}
		}
		for (LexNode lex : collect) {
			if (map.containsKey(lex.getPhrase())) {
				map.get(lex.getPhrase()).add(lex.getType());
			}
			else {
				Set<CCGType> types = new HashSet<CCGType>();
				types.add(lex.getType());
				map.put(lex.getPhrase().toLowerCase(), types);
			}
		}
		for (Entry<String, Set<CCGType>> pair : map.entrySet()) {
			//code means that we've found some shit that's not real!
			if (pair.getValue().contains("CODE")) {
				//continue; this shouldn't be a problem anymore, but I want to double check
				System.err.println("Parsing algorithm captures CODES");
			}
			if (pair.getValue().contains("TOP")) {
				continue; //not sure if this is generally right... but we'll try it. 
			}
			addEquivalences(pair.getValue(), equivalences);
		}
		for (String key : map.keySet()) {
			try {
				threadAggregate.putOrCombine(key, new TypeCombine(map.get(key)));
			} catch (CombineException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
		
	}
	private void addEquivalences(Set<CCGType> types, Map<CCGType, Set<CCGType>> equivalences) {
		for (CCGType s : equivalences.keySet()) {
			if (types.contains(s.toString())) {
				for (CCGType cg : equivalences.get(s)) {
					types.add(cg);
				}
			}
		}
	}
	private ParseNode grokTypeRaise(ParseNode pn, Map<CCGType, Set<CCGType>> equivalences) {
		if (!(pn instanceof RuleNode)) {
			return pn;
		}
		RuleNode rn = (RuleNode) pn;
		if (rn.getRule() == SyntaxRuleType.TypeRaise || rn.getRule() == SyntaxRuleType.TCR || rn.getRule() == SyntaxRuleType.TPC) {
		//	System.out.println("Grokking type raise");
			if (!equivalences.containsKey(rn.getLeftChild().getType())) {
				equivalences.put(rn.getLeftChild().getType(), new HashSet<CCGType>());
			}
			if (!equivalences.containsKey(rn.getType())) {
				equivalences.put(rn.getType(), new HashSet<CCGType>());
			}
			equivalences.get(rn.getLeftChild().getType()).add(rn.getType());
			equivalences.get(rn.getType()).add(rn.getLeftChild().getType());
			if (rn.getLeftChild() instanceof RuleNode) {
				//dam this sux.
			//	System.err.println("Purity test failed");
				return rn;
			}
			return new LexNode(rn.getPhrase(), rn.getType());
		}
		return rn;
	}
	private ParseNode grokPCT(ParseNode pn) {
		if (!(pn instanceof RuleNode)) {
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
				System.exit(1);
			}
		}
		return rn;
	}

	@Override
	public void reduce(WordMap threadAggregate) {
		synchronized(processAggregate) {
			processAggregate.combine(threadAggregate);
		}
	}

}
