package util.listdata;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import util.data.Comment;
import util.data.CommentFormat;
import util.json.JsonReadable;
import util.sys.DataType;

public class KeywordList extends DataCollection<Comment> {
	private static final long serialVersionUID = 6441745759936713041L;
	private final CommentFormat cf;
	public KeywordList() { // dummy constructor
		super();
		this.cf = null;
	}
	public KeywordList(String[] keywords, CommentFormat cf) {
		super(keywords);
		this.cf = null;
	}
	public KeywordList(KeywordList kl) {
		super(kl);
		this.cf = kl.cf;
	}
	public KeywordList(List<String> fileLines, CommentFormat cf) {
		super(fileLines);
		this.cf = cf;
	}
	@Override
	public boolean hasNArgs() {
		return true;
	}
	public static KeywordList createFromFile(File f, CommentFormat cf) {
		List<String> lines = null;;
		try {
			lines = Files.readAllLines(f.toPath());
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		String thisCreateLine = lines.get(0);
		if (!isKeyLine(thisCreateLine)) {
			System.err.println("These lists have still not been 'fixed', please run ReorderListProcessor");
			System.exit(1);
		}
		return new KeywordList(lines, cf);
	}

	@Override
	public String getConstructionErrorMsg() {
		return "KeywordList requires one or more keywords";
	}
	@Override
	public DataType deepCopy() {
		return new KeywordList(this);
	}
	@Override
	protected Collection<Comment> getEmptyCollection() {
		return new ArrayList<Comment>();
	}
	@Override
	protected Comment getValue(Comment c) {
		return c;
	}
	public List<String> getRelevantKeywords(String text) {		
		List<String> retval = new ArrayList<String>();
		if (text.isEmpty()) {
			return retval;
		}
		for (String key : this.keySet()) {
			if (text.contains(key)) {
				retval.add(key);
			}
		}
		return retval;
	}
	@Override
	protected Comment parseValue(String s) {
		JsonReadable jr = JsonReadable.fromString(s);
		return cf.getComment(jr);
	}


}
