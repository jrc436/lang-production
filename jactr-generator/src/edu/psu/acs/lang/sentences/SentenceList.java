package edu.psu.acs.lang.sentences;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import util.sys.DataType;
import util.sys.FileWritable;

public class SentenceList extends HashMap<Integer, String> implements DataType {
	/**
	 * 
	 */
	public static final String delim = ":S:P:";
	private static final long serialVersionUID = 7610951695401883228L;

	public SentenceList() {
		super();
	}
	public static SentenceList readFile(File f) throws IOException {
		return new SentenceList(Files.readAllLines(f.toPath()));
	}
	public SentenceList(SentenceList s) {
		super(s);
	}
	public SentenceList(List<String> readAllLines) {
		for (String line : readAllLines) {
			if (!line.contains(delim)) {
				System.err.println(line);
				throw new IllegalArgumentException("These lines don't come from a proper SentenceList");
			}
			String[] parts = line.split(delim);
			super.put(Integer.parseInt(parts[0]), parts[1]);
		}
	}
	public void addAll(SentenceList other) {
		for (Integer i : other.keySet()) {
			this.put(i, other.get(i));
		}
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
		return "SentenceDataType requires no args";
	}

	@Override
	public String getFileExt() {
		return ".list";
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> lines = new ArrayList<String>();
		for (Entry<Integer, String> entry : this.entrySet()) {
			lines.add(entryString(entry.getKey()));
		}
		return lines;
	}
	
	private String entryString(Integer en) {
		String value = this.containsKey(en) ? this.get(en) : "";
		return en + delim + value;
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
		int maxValue = 0;
		for (Integer i : this.keySet()) {
			maxValue = Math.max(maxValue, i);
		}
		List<Integer> intList = new ArrayList<Integer>();
		for (int i = 1; i < maxValue; i++) {
			intList.add(i);
		}
		return FileWritable.<Integer, List<Integer>>iterBuilder(intList, this::entryString);
	}

	@Override
	public DataType deepCopy() {
		return new SentenceList(this);
	}

}
