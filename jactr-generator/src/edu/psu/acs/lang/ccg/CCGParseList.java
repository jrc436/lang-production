package edu.psu.acs.lang.ccg;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import edu.psu.acs.lang.parsing.NodeParser;
import edu.psu.acs.lang.parsing.ParseException;
import edu.psu.acs.lang.parsing.ParseNode;
import util.sys.DataType;
import util.sys.FileWritable;

public class CCGParseList implements DataType {
	private final NodeParser ccglist;
	public CCGParseList() {
		this.ccglist = new NodeParser();
	}
	public CCGParseList(Path f, boolean originalFiles) throws ParseException  {
		NodeParser np = null;
		np = new NodeParser(f, originalFiles);
		this.ccglist = np;
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
	public NodeParser getParser() {
		return ccglist;
	}
	

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> lines = new ArrayList<String>();
		for (ParseNode cg : ccglist.getTops()) {
			lines.add(cg.toString());
		}
		return lines;
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
