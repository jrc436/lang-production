package edu.psu.acs.lang.lexsyn;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.psu.acs.lang.declarative.type.CCGCompoundType;
import edu.psu.acs.lang.declarative.type.CCGType;
import edu.psu.acs.lang.parsing.ParseException;
import util.collections.DoubleKeyMap;
import util.collections.Pair;
import util.sys.DataType;
import util.sys.FileWritable;

public class LexsynOrderedList extends DoubleKeyMap<String, CCGType, Integer> implements DataType {
	public static final String betweenTypes = ":-:";
	private Map<String, List<CCGType>> ordering = null;
	public LexsynOrderedList() {
		super();
	}
	public LexsynOrderedList(LexsynOrderedList lol) {
		super(lol);
	}
	public LexsynOrderedList(DoubleKeyMap<String, CCGType, Integer> dkm) {
		super(dkm);
	}
	
	public static LexsynOrderedList createFromDir(Path p) throws IOException, ParseException {
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
	public static LexsynOrderedList createFromFile(Path p) throws IOException, ParseException {
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
	private Map<String, List<CCGType>> collapseOrder() {
		Map<String, List<CCGType>> map = new HashMap<String, List<CCGType>>();
		for (String word : this.getKeysetOne()) {
			List<CCGType> ordering = produceOrdering(word);
			map.put(word, new ArrayList<CCGType>());
			for (int i = 0; i < ordering.size(); i++) {
				map.get(word).add(ordering.get(i));
			}
		}
		return map;
	}
	public void collapse() {
		this.ordering = collapseOrder();
	}
	public Map<String, Set<CCGType>> getFirst(int number) {
		Map<String, List<CCGType>> map = collapseOrder();
		Map<String, Set<CCGType>> use = new HashMap<String, Set<CCGType>>();
		for (String word : map.keySet()) {
			use.put(word, new HashSet<CCGType>());
			for (int i = 0; i < number && i < map.get(word).size(); i++) {
				use.get(word).add(map.get(word).get(i));
			}
		}
		return use;
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
		List<CCGType> ordered = new ArrayList<CCGType>(this.getPairedKeys(word));
		LexsynOrderedList outer = this;
		Comparator<CCGType> compare = new Comparator<CCGType>() {
			@Override
			public int compare(CCGType arg0, CCGType arg1) {
				return outer.compare(word, arg0, arg1);
			}
		};
		Collections.sort(ordered, compare);
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
	private String fastEntryString(String word) {
		if (this.ordering == null) {
			System.err.println("Warn: slow entryString due to uncollapsed list");
			return entryString(word);
		}
		String add = word;
		for (CCGType c : ordering.get(word)) {
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
		collapse();
		return FileWritable.<String, Set<String>>iterBuilder(this.getKeysetOne(), this::fastEntryString);
		
	}

	@Override
	public DataType deepCopy() {
		return new LexsynOrderedList(this);
	}

}
