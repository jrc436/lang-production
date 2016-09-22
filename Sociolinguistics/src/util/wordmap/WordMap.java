package util.wordmap;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import filter.StringCleaner;
import util.data.Comment;
import util.data.CommentFormat;
import util.sys.DataType;
import util.sys.FileWritable;

public class WordMap extends HashMap<String, Combinable> implements DataType {
	private static final long serialVersionUID = 6844547921098526441L;
	//the splitter should be something very unlikely to be in the string
	public static final String splitter = "::-:--:";
	private final Set<Class<? extends Combinable>> classes;
	public WordMap(Set<Class<? extends Combinable>> classes) {
		super();
		this.classes = classes;
	}
	/**
	 * This constructor won't really work, except to access some factory methods
	 */
	public WordMap() {
		super();
		this.classes = new HashSet<Class<? extends Combinable>>();
	}
	@SuppressWarnings("unchecked")
	public WordMap(String[] clses) {
		super();
		classes = new HashSet<Class<? extends Combinable>>();
		for (String cls : clses) {
			try {
				classes.add((Class<? extends Combinable>) Class.forName(cls));
			} catch (ClassNotFoundException e) {
				System.err.println("Class: "+cls+" not found or class doesn't implement Combinable");
				System.exit(1);
			}
		}
	}
	public static WordMap createFromDirectory(String fp) {
		File[] wmFiles = Paths.get(fp).toFile().listFiles();
		WordMap agg = null;
		for (File f : wmFiles) {
			WordMap wm = createFromFile(f);
			if (agg == null) {
				agg = wm;
			}
			else {
				agg.combine(wm);
			}
		}
		return agg;
	}
	public static WordMap createFromFile(File f) {
		List<String> lines = null;;
		try {
			lines = Files.readAllLines(f.toPath());
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		String thisCreateLine = lines.remove(0);
		return create(lines, thisCreateLine);
	}
	public static WordMap create(List<String> lines, String createLine) {
		WordMap wm = new WordMap(createLine);
		for (String s : lines) {
			wm.addFromString(s);
		}
		return wm;
	}
	public WordMap(String headerLine) {
//		String[] parts = headerLine.split(headSplit);
		this.classes = Combinable.fromString(headerLine);
	}
	@SafeVarargs
	public WordMap(CommentFormat cmf, Class<? extends Combinable>...cls) {
		super();
		Set<Class<? extends Combinable>> classes = new HashSet<Class<? extends Combinable>>();
		for (Class<? extends Combinable> c : cls) {
			classes.add(c);
		}
		this.classes = classes;
	}
	public Combinable getBy(String key, Class<? extends Combinable> comb) {
		if (!classes.contains(comb)) {
			return null;
		}
		Combinable c = super.get(key);
		if (c.getClass().equals(comb)) {
			return c;
		}
		else if (c instanceof CombineSet) {
			CombineSet cs = (CombineSet)c;
			return cs.get(comb);
		}
		throw new IllegalStateException("The class isn't a combine set, or the class, but it does contain it. Something is mismatched");
	}
	public void putOrCombine(String key, Combinable c) throws CombineException {
		if (!classes.contains(c.getClass())) {
			throw new IllegalArgumentException("Cannot combine with a combinable that isn't part of this wordmap");
		}
		if (!super.containsKey(key)) {
			super.put(key, c);
			return;
		}
		Combinable cother = super.get(key);
		if (!cother.getClass().equals(c.getClass())) {
			this.put(key, cother.combine(c));
		}
		else if (cother instanceof CombineSet) {
			CombineSet cs = (CombineSet)cother;
			cs.set(c.getClass(), cs.get(c.getClass()).combine(c));
		}
		throw new IllegalStateException("The class isn't a combine set, or the class, but it does contain it. Something is mismatched");
	}
		
	public synchronized void combine(WordMap other) {
		for (String word : other.keySet()) {
			if (this.containsKey(word)) {
				try {
					this.put(word, this.get(word).combine(other.get(word)));
				} catch (CombineException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
			else {
				this.put(word, other.get(word));
			}
		}
	}
//	public Comment getAsComment(JsonReadable j) {
//		return cmf.getComment(j);
//	}
	public synchronized void putWord(String word, Comment data) {
		Combinable initial = Combinable.populate(classes, data);
		if (this.containsKey(word)) {
			try {
				this.put(word, this.get(word).combine(initial));
			} catch (CombineException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
		else {
			this.put(word, initial);
		}	
	}
	public synchronized void addCommentText(Comment s) {
		String[] words = s.getText().split("\\s+");
		for (String word : words) {
			word = StringCleaner.cleanWord(word);
			Combinable initial = Combinable.populate(classes, s);
			if (word.isEmpty()) {
				continue;
			}
			if (this.containsKey(word)) {
				try {
					this.put(word, this.get(word).combine(initial));
				} catch (CombineException e) {
					e.printStackTrace();
					System.exit(1);
				}
			}
			else {
				this.put(word, initial);
			}
		}
	}
	//have to update this to use class set
	public synchronized void addFromString(String s) {
		try {
			int splitdex = s.lastIndexOf(splitter);
			String str = s.substring(0, splitdex);
			String comb = s.substring(splitdex + splitter.length());
			this.put(str, Combinable.recreate(classes, comb));
		}
		catch (Exception e) {
			e.printStackTrace();
			System.out.println("Trouble adding to WordMap:"+s);
			System.exit(1);
		}
		//System.out.println("Successful add");
	}
	private String getClsHeadString() {
		return CombineSet.mockClsString(this.classes);
	}
//	private static final String headSplit = "!uiu!";
//	private String getFullHeader() {
//		return cmf.toString() + headSplit + getClsHeadString();
//	}
//	
	//have to update the toString of CombineSet
	private String entryString(Entry<String, Combinable> entry) {
		 return entry.getKey()+splitter+entry.getValue().toString();
	}
	@Override
	public String getFileExt() {
		return ".wm";
	}
	@Override
	public int getNumFixedArgs() {
		return 0;
	}
	@Override
	public boolean hasNArgs() {
		return true;
	}
	@Override
	public String getConstructionErrorMsg() {
		return "Arguments to WordMap should be fully qualified class names extending Combinable";
	}
	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> v = new ArrayList<String>();
		for (Entry<String, Combinable> entry : this.entrySet()) {
			v.add(this.entryString(entry));
		}
		return v;
	}
	@Override
	public String getHeaderLine() {
		return this.getClsHeadString();
	}
	@Override
	public String getFooterLine() {
		return null;
	}
	@Override
	public DataType deepCopy() {
		WordMap wm = new WordMap(this.classes);
		for (String key : this.keySet()) {
			wm.put(key, this.get(key));
		}
		return wm;
	}
//	@Override
//	public boolean isFull(int gbAllocated) {
//		return this.size() > 10000*gbAllocated;
//	}
	@Override
	public Iterator<String> getStringIter() {
		return FileWritable.<WordMap.Entry<String, Combinable>, Set<Entry<String, Combinable>>>iterBuilder(this.entrySet(), this::entryString);
	}

}
