package util.listdata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import util.data.Comment;
import util.sys.DataType;

public abstract class DataCollection<E> extends HashMap<String, Collection<E>> implements DataType {

	private static final long serialVersionUID = 1046358937480295913L;
	
	public DataCollection() { // dummy constructor
		super();
	}
	public DataCollection(String[] keywords) {
		super();
		for (String key : keywords) {
			super.put(key, getEmptyCollection());
		}
	}
//	public DataCollection(CommentFormat cf) {
//		super();
//		this.cf = cf;
//	}
	public DataCollection(DataCollection<E> kl) {
		super(kl);
		//this.cf = kl.cf;
	}
	protected DataCollection(List<String> fileLines) {
		super();
		//this.cf = cf;
		if (fileLines.size() == 0 || !isKeyLine(fileLines.get(0))) {
			throw new IllegalArgumentException("There needs to be at least one key line starting the file.");
		}
		String currentKeyLine = null;
		for (String s : fileLines) {
			currentKeyLine = addLine(s, currentKeyLine);
		}
	}
//	protected static String getTrailingKeyword(List<String> fileLines) {
//		String currentKeyLine = "";
//		if (!isKeyLine(fileLines.get(0))) {
//			System.err.println("Warning: lines have no initial keyline");
//		}
//		for (String s : fileLines) {
//			if (isKeyLine(s)) {
//				currentKeyLine = s;
//			}
//		}
//		return currentKeyLine;
//	}
	
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
		return "DataCollections vary on the number of arguments they specify. If you are seeing this message, it's possible your collection is not fully defined";
	}

	@Override
	public String getFileExt() {
		return ".list";
	}
	public void add(String keyword, E datum) {
		if (!this.containsKey(keyword)) {
			this.put(keyword, getEmptyCollection());
		}
		this.get(keyword).add(datum);
	}
	public void addComment(String keyword, Comment datum) {
		add(keyword, getValue(datum));
	}
	protected abstract Collection<E> getEmptyCollection();
	public Collection<E> getCopyCollection(String key) {
		Collection<E> fe = getEmptyCollection();
		fe.addAll(this.get(key));
		return fe;
	}
	
	private static final String init = "?-? ";
	private static final String out = " !-!";
	
	private static String getKeyMarker(String origLine) {
		return init + origLine + out;
	}
	public static boolean isKeyLine(String line) {
		return line.length() >= init.length() + out.length() && line.substring(0, init.length()).equals(init) && line.substring(line.length()-out.length()).equals(out);
	}
	private static String returnFromMarker(String line) {
		return line.substring(init.length(), line.length()-out.length());
	}
	private String addLine(String s, String currentKeyLine) {
		if (isKeyLine(s)) {
			super.put(returnFromMarker(s), getEmptyCollection());
			currentKeyLine = returnFromMarker(s);
		}
		else {
			super.get(currentKeyLine).add(parseValue(s));
		}
		return currentKeyLine;
	}
	protected abstract E getValue(Comment c);
	protected abstract E parseValue(String s);

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> lines = new ArrayList<String>();
		for (String key : this.keySet()) {
			lines.add(getKeyMarker(key));
			for (E c : this.get(key)) {
				lines.add(c.toString());
			}
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
		Iterator<String> keys = this.keySet().iterator();
		final DataCollection<E> outer = this;
		Iterator<String> iter = new Iterator<String>() {
			private Iterator<E> vals;
			
			public boolean hasNext() {
				return keys.hasNext() || (vals != null && vals.hasNext());
			}
			public String next() {
				if ((vals == null || !vals.hasNext()) && keys.hasNext()) {
					String key = keys.next();
					vals = outer.get(key).iterator();
					return getKeyMarker(key);
				}
				else if (vals != null && vals.hasNext()) { //we're going to return, so yeah!
					return vals.next().toString();
				} //neither keys nor vals have a next
				throw new NoSuchElementException();
			}
		};
		return iter;
	}
}
