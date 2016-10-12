package edu.psu.acs.lang.eval.data.garblegold;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import edu.psu.acs.lang.output.tree.GarbleTree;
import edu.psu.acs.lang.output.tree.GarbleTreeList;
import edu.psu.acs.lang.sentences.SentenceList;
import util.sys.DataType;
import util.sys.FileWritable;

public class GarbleGoldList extends HashMap<Integer, GarbleGold> implements DataType {
	@FunctionalInterface
	private interface ReadLine {
		void addLine(String s);
	}
	public GarbleGoldList() {
		super();
	}
	public GarbleGoldList(GarbleGoldList ggl) {
		super(ggl);
	}
	private static void readFile(File f, ReadLine rl) throws IOException {
		List<String> lines = Files.readAllLines(f.toPath());
		for (String line : lines) {
			rl.addLine(line);
		}
	}
	public static GarbleGoldList readAnyFile(File f) throws IOException {
		GarbleGoldList ggl = new GarbleGoldList();
		readFile(f, ggl::addAnyLine);
		return ggl;
	}
	public static GarbleGoldList readSFile(File f) throws IOException {
		GarbleGoldList ggl = new GarbleGoldList();
		readFile(f, ggl::addSentenceLine);
		return ggl;
	}
	public static GarbleGoldList readGGFile(File f) throws IOException {
		GarbleGoldList ggl = new GarbleGoldList();
		readFile(f, ggl::addGGLine);
		return ggl;
	}
	public static GarbleGoldList readGTFile(File f) throws IOException {
		GarbleGoldList ggl = new GarbleGoldList();
		readFile(f, ggl::addGarbleLine);
		return ggl;
	}
	private static final long serialVersionUID = -8032374280142484964L;

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
		return "GarbleGoldList requires no further arguments";
	}

	@Override
	public String getFileExt() {
		return ".gglist";
	}
	
	private static final String delim = "::num::garblegold";

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> lines = new ArrayList<String>();
		for (Integer i : this.keySet()) {
			lines.add(i.toString()+delim+this.get(i).toString());
		}
		return lines;
	}
	private void addAnyLine(String line) {
		if (line.contains(SentenceList.delim)) {
			addSentenceLine(line);
		}
		else if (line.contains(GarbleTreeList.delim)) {
			addGarbleLine(line);
		}
		else if (line.contains(GarbleGoldList.delim)) {
			addGGLine(line);
		}
		else {
			throw new IllegalArgumentException("line: "+line+" is neither a sentence, garbletree, or garblegold");
		}
	}
	private void addSentenceLine(String line) {
		String[] parts = line.split(SentenceList.delim);
		String sent = parts.length == 2 ? parts[1] : "";
		putSentence(Integer.parseInt(parts[0]), sent);
	}
	private void addGarbleLine(String line) {
		String[] parts = line.split(GarbleTreeList.delim);
		putGarble(Integer.parseInt(parts[0]), GarbleTree.fromString(parts[1]));
	}
	public GarbleGold putSentence(Integer i, String s) {
		return this.put(i, new GarbleGold(null, s));
	}
	public GarbleGold putGarble(Integer i, GarbleTree gt) {
		return this.put(i, new GarbleGold(gt, null));
	}
	public void absorb(GarbleGoldList other) {
		for (Integer i : other.keySet()) {
			this.put(i, other.get(i));
		}
	}
	@Override
	public GarbleGold put(Integer i, GarbleGold gg) {
		if (this.containsKey(i)) {
			if (this.get(i).complete()) {
				throw new IllegalArgumentException("There seems to be duplicate data surrounding sentence: "+i);
			}
			this.get(i).combine(gg);
			return this.get(i);
		}
		return super.put(i,  gg);
	}
	private void addGGLine(String line) {
		String[] parts = line.split(delim);
		if (parts.length != 2) {
			throw new IllegalArgumentException("line: "+line+" is not a valid garblegold line");
		}
		super.put(Integer.parseInt(parts[0]), GarbleGold.fromString(parts[1]));
	}

	@Override
	public String getHeaderLine() {
		return null;
	}

	@Override
	public String getFooterLine() {
		return null;
	}
	private String entryString(Integer i) {
		return i + delim + this.get(i).toString();
	}

	@Override
	public Iterator<String> getStringIter() {
		List<Integer> sortedKeys = new ArrayList<Integer>(this.keySet());
		Collections.sort(sortedKeys);
		return FileWritable.<Integer, List<Integer>>iterBuilder(sortedKeys, this::entryString);
	}

	@Override
	public DataType deepCopy() {
		return new GarbleGoldList(this);
	}

}
