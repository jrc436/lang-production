package edu.psu.acs.lang.sentences;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import edu.psu.acs.lang.ccg.CCGParseList;
import edu.psu.acs.lang.parsing.ParseException;
import edu.psu.acs.lang.parsing.ParseNode;
import util.sys.FileProcessor;

public class SentenceListCreator extends FileProcessor<CCGParseList, SentenceList> {

	public SentenceListCreator() {
		super();
	}
	public SentenceListCreator(String inputDir, String outputDir) {
		super(inputDir, outputDir, new SentenceList());
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
		return "SentenceListCreator requires no arguments.";
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
	private static Integer key = 1;
	private static final List<String> tailPastPhrases = new ArrayList<String>();
	@Override
	public void map(CCGParseList newData, SentenceList threadAggregate) {
		String pastPhrase = "";
		for (ParseNode pn : newData.getParser().getTops()) {
			String phrase = pn.getPhrase();
			if (phrase.equals(pastPhrase)) {
				continue;
			}
			else if (phrase.contains(",") || phrase.contains("?") || phrase.contains(".") || phrase.contains(":") || phrase.contains("!")) {
				continue;
			}
			else if (tailPastPhrases.contains(phrase)) {
				continue; //this isn't *horribly* unlikely
			}
			synchronized(key) {
				threadAggregate.put(key, phrase);
				key++;
			}
			pastPhrase = phrase;
		}
		synchronized(tailPastPhrases) {
			tailPastPhrases.add(pastPhrase);
		}
	}

	@Override
	public void reduce(SentenceList threadAggregate) {
		synchronized(processAggregate) {
			for (Entry<Integer, String> en : threadAggregate.entrySet()) {
				processAggregate.put(en.getKey(), en.getValue());
			}
		}
	}

}
