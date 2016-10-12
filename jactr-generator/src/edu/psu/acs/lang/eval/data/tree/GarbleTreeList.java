package edu.psu.acs.lang.eval.data.tree;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.psu.acs.lang.output.OutputReader;
import edu.psu.acs.lang.output.OutputSentence;
import util.sys.DataType;
import util.sys.FileWritable;

public class GarbleTreeList extends HashMap<Integer, GarbleTree> implements DataType {

	public GarbleTreeList() {
		super();
	}
	public GarbleTreeList(GarbleTreeList gtl) {
		super(gtl);
	}
	protected GarbleTreeList(Map<Integer, GarbleTree> gtl) {
		super(gtl);
	}
	public static GarbleTreeList readOutputFile(File f) throws IOException {
		Map<Integer, OutputSentence> os = OutputReader.readFile(f);
		GarbleTreeList gtl = new GarbleTreeList();
		for (Entry<Integer, OutputSentence> en : os.entrySet()) {
			gtl.put(en.getKey(), new GarbleTree(en.getValue()));
		}
		return gtl;
	}
	public void absorb(GarbleTreeList other) {
		for (Integer key : other.keySet()) {
			if (this.containsKey(key)) {
				//uh oh..
				if (this.get(key).equals(other.get(key))) {
					System.err.println("Warning: rejecting duplicate add");
				}
				else {
					//double uh oh
					System.err.println("Inconsistent values for key: "+key);
					throw new RuntimeException("this: "+this.get(key).toString()+"; other: "+other.get(key).toString());
				}
			}
			else {
				this.put(key,  other.get(key));
			}
		}
	}
	public static GarbleTreeList readTreeFile(File f) throws IOException {
		List<String> lines = Files.readAllLines(f.toPath());
		Map<Integer, GarbleTree> map = new HashMap<Integer, GarbleTree>();
		for (String line : lines) {
			String[] parts = line.split(delim);
			if (parts.length != 2) {
				throw new IllegalArgumentException("File: "+f+" is not a Tree file");
			}
			map.put(Integer.parseInt(parts[0]), GarbleTree.fromString(parts[1]));
		}
		return new GarbleTreeList(map);
	}
	private static final long serialVersionUID = -5772705013087039637L;
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
		return "GarbleTree requires no further arguments";
	}
	@Override
	public String getFileExt() {
		return ".gtree";
	}
	@Override
	public ArrayList<String> getDataWriteLines() {
		return null;
	}
	@Override
	public String getHeaderLine() {
		return null;
	}
	@Override
	public String getFooterLine() {
		return null;
	}
	public static final String delim = "G::D";
	private String entryString(Entry<Integer, GarbleTree> entry) {
		return entry.getKey()+delim+entry.getValue().toString();
	}
	@Override
	public Iterator<String> getStringIter() {
		 return FileWritable.<Entry<Integer, GarbleTree>, Set<Entry<Integer, GarbleTree>>>iterBuilder(this.entrySet(), this::entryString);
	}
	@Override
	public DataType deepCopy() {
		return new GarbleTreeList(this);
	}

}
