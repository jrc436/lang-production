package edu.psu.acs.lang.lexsyn;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import edu.psu.acs.lang.util.NodeParser;
import edu.psu.acs.lang.util.ParseException;
import edu.psu.acs.lang.util.ParseNode;
import util.sys.DataType;
import util.sys.FileWritable;

public class CCGParseList implements DataType {
	private final NodeParser ccglist;
	public CCGParseList() {
		this.ccglist = null;
	}
	public CCGParseList(List<String> fileLines, boolean originalFiles) throws ParseException {
		List<String> ccgTerms = gatherCCG(fileLines);
		this.ccglist = new NodeParser(ccgTerms, originalFiles);
	}
	public CCGParseList(CCGParseList list) {
		this.ccglist = new NodeParser(list.ccglist);
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
		return "CCGList requires no arguments";
	}

	@Override
	public String getFileExt() {
		return ".ccg";
	}
	public List<ParseNode> getTops() {
		return ccglist.getTops();
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> lines = new ArrayList<String>();
		for (ParseNode cg : ccglist.getTops()) {
			lines.add(cg.toString());
		}
		return lines;
	}
	private List<String> gatherCCG(List<String> lines) {
		List<String> allCCG = new ArrayList<String>();
		for (int i = 0; i < lines.size(); i++) {
			if (lines.get(i).contains("NEW SENTENCE") && !lines.get(i+2).contains("CODE")) {
				allCCG.add(readOneCCG(lines, i));
			}	
		}
		return allCCG;
	}
	private String readOneCCG(List<String> lines, int startingIndex) {
		String line = lines.get(startingIndex);
		String toRet = line+System.getProperty("line.separator");
		int j = startingIndex+1;
		line = lines.get(j);
		while (!line.contains("NEW SENTENCE")) {
			toRet += (line+System.getProperty("line.separator"));
			j++;
			if (j == lines.size()) {
				break;
			}
			line = lines.get(j);
		}
		return toRet;
	}

	@Override
	public String getHeaderLine() {
		return null;
	}

	@Override
	public String getFooterLine() {
		return null;
	}

	@Override
	public Iterator<String> getStringIter() {
		return FileWritable.<ParseNode, List<ParseNode>>iterBuilder(this.ccglist.getTops());
	}

	@Override
	public DataType deepCopy() {
		return new CCGParseList(this);
	}
}
