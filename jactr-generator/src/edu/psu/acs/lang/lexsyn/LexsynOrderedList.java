package edu.psu.acs.lang.lexsyn;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import edu.psu.acs.lang.declarative.CCGCompoundType;
import edu.psu.acs.lang.declarative.CCGType;
import util.collections.DoubleKeyMap;
import util.collections.Pair;
import util.sys.DataType;
import util.sys.FileWritable;

public class LexsynOrderedList extends DoubleKeyMap<String, CCGType, Integer> implements DataType {
	public static final String betweenTypes = ":-:";
	public LexsynOrderedList() {
		super();
	}
	public LexsynOrderedList(LexsynOrderedList lol) {
		super(lol);
	}
	public LexsynOrderedList(DoubleKeyMap<String, CCGType, Integer> dkm) {
		super(dkm);
	}
	public static LexsynOrderedList createFromDir(Path p) throws IOException {
		if (!p.toFile().isDirectory()) {
			System.err.println(p);
			throw new IllegalArgumentException("This doesn't point to a valid directory");
		}
		LexsynOrderedList base = new LexsynOrderedList();
		for (File f : p.toFile().listFiles()) {
			base.absorb(createFromFile(f.toPath()));
		}
		return base;
	}
	public static LexsynOrderedList createFromFile(Path p) throws IOException {
		List<String> lines = Files.readAllLines(p);
		LexsynOrderedList toReturn = new LexsynOrderedList();
		for (String line : lines) {
			String[] parts = line.split(betweenTypes);
			if (parts.length == 0) {
				System.err.println(p);
				throw new IllegalArgumentException("This doesn't point to a valid dsv file");
			}
			String word = parts[0];
			for (int i = 1; i < parts.length; i++) {
				toReturn.add(word, CCGCompoundType.makeCCGType(parts[i], true));
			}
		}
		return toReturn;
	}
	public HashMap<String, Set<CCGType>> getFirst(int number) {
		HashMap<String, Set<CCGType>> map = new HashMap<String, Set<CCGType>>();
		for (String word : this.getKeysetOne()) {
			List<CCGType> ordering = produceOrdering(word);
			map.put(word, new HashSet<CCGType>());
			for (int i = 0; i < number; i++) {
				map.get(word).add(ordering.get(i));
			}
		}
		return map;
	}
	public void add(String word, CCGType type) {
		int addend = this.containsKey(word, type) ? this.get(word, type) : 0;
		this.put(word, type, addend + 1);
	}
	public void absorb(LexsynOrderedList other) {
		for (Pair<String, CCGType> keys : other.keySet()) {
			int addend = this.containsKey(keys) ? this.get(keys) : 0;
			this.put(keys, addend + other.get(keys));
		}
	}
	
	private List<CCGType> produceOrdering(String word) {
		List<CCGType> ordered = new ArrayList<CCGType>();
		for (CCGType key : this.getFirstPairedKeys(word)) {
			boolean added = false;
			for (int i = 0; i < ordered.size(); i++) {
				if (compare(word, key, ordered.get(i)) < 0) {
					ordered.add(i, key);
				}
			}
			if (!added) {
				ordered.add(key);
			}
		}
		return ordered;
	}
	private int compare(String word, CCGType one, CCGType two) {
		return this.get(word, one).compareTo(this.get(word, two));
	}
	private static final long serialVersionUID = -4278083817576157027L;

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
		return "LexsynOrderedList requires no arguments";
	}

	@Override
	public String getFileExt() {
		return ".dsv";
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> retval = new ArrayList<String>();
		for (String word : this.getKeysetOne()) {
			retval.add(entryString(word));
		}
		return retval;
	}
	private String entryString(String word) {
		List<CCGType> ordered = this.produceOrdering(word);
		String add = word;
		for (CCGType c : ordered) {
			add += betweenTypes + c.toString();
		}
		return add;
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
		return FileWritable.<String, List<String>>iterBuilder(this.getKeysetOne(), this::entryString);
		
	}

	@Override
	public DataType deepCopy() {
		return new LexsynOrderedList(this);
	}

}
