package edu.psu.acs.lang.ccg;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Set;

import edu.psu.acs.lang.declarative.type.CCGType;
import edu.psu.acs.lang.lexsyn.LexsynOrderedList;
import edu.psu.acs.lang.parsing.NodeParser;
import edu.psu.acs.lang.parsing.ParseException;
import edu.psu.acs.lang.parsing.ParseNode;
import util.collections.DoubleKeyMap;
import util.sys.FileProcessor;

public class CCGParseListFilter extends FileProcessor<CCGParseList, CCGParseList> {
	private final int maxSentenceLength;
	private final Map<String, Set<CCGType>> lol;
	public CCGParseListFilter() {
		this.maxSentenceLength = -1;
		this.lol = null;
	}
	public CCGParseListFilter(String inpDir, String outDir, String[] args) {
		super(inpDir, outDir, new CCGParseList());
		maxSentenceLength = Integer.parseInt(args[1]);
		int maxTypesPerWord = Integer.parseInt(args[2]);
		LexsynOrderedList toadd = null;
		try {
			toadd = LexsynOrderedList.createFromDir(Paths.get(args[0]));
		}
		catch (IOException |ParseException ie) {
			ie.printStackTrace();
			System.exit(1);
		} 
		lol = toadd.getFirst(maxTypesPerWord);
	}
	
	@Override
	public int getNumFixedArgs() {
		return 3; //max length per sentence and max types and path to orderedlist
	}

	@Override
	public boolean hasNArgs() {
		return false;
	}

	@Override
	public String getConstructionErrorMsg() {
		return "After the input and outputdirectory, the CCGParseListFilter should first take the filepath to the lexsyns/wordsinfo, then the maximum number of words in a sentence, and then the maximum number of types per word.";
	}

	@Override
	public CCGParseList getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		try {
			return new CCGParseList(f.toPath(), false);
		} catch (ParseException e) {
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}

	@Override
	public void map(CCGParseList newData, CCGParseList threadAggregate) {
		for (ParseNode pn : newData.getParser().getTops()) {
			boolean add = true;
			String phrase = pn.getPhrase();
			String[] words = phrase.split(" ");
			if (words.length > maxSentenceLength) {
				add = false;
			}
			DoubleKeyMap<String, CCGType, Integer> nodeTypes = NodeParser.wordTypes(pn);
			for (String word : words) {
				for (CCGType types : nodeTypes.getPairedKeys(word)) {
					if (!lol.get(word).contains(types)) {
						add = false;
						break;
					}
				}
				if (!add) {
					break;
				}
			}
			if (add) {
				threadAggregate.getParser().addTop(pn);
			}
		}
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
