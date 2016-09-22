package util.listdata;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import util.data.Comment;
import util.sys.DataType;

public class UserList extends DataCollection<String> {

	private static final long serialVersionUID = -8767860454998293903L;
	
	public UserList() { // dummy constructor
		super();
	}
	public UserList(String[] keywords) {
		super(keywords);
	}
//	public UserList(CommentFormat cf) {
//		super(cf);
//	}
	public UserList(UserList kl) {
		super(kl);
	}
	public UserList(List<String> fileLines) {
		super(fileLines);
	}
	public static UserList createFromFile(File f) {
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
		return new UserList(lines);
	}
	
	@Override
	public DataType deepCopy() {
		return new UserList(this);
	}

	@Override
	protected String getValue(Comment c) {
		return c.getAuthor();
	}
	@Override
	protected Collection<String> getEmptyCollection() {
		return new HashSet<String>();
	}
	@Override
	protected String parseValue(String s) {
		return s;
	}

}
