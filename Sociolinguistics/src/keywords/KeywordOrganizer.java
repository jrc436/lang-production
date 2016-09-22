package keywords;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import filter.StringCleaner;

@Deprecated
public class KeywordOrganizer {
	private final Set<String> keywords;
	private final String name;
	private final FileWriter fw;
	public KeywordOrganizer(Path outFolder, String name, String...keywords) throws IOException {
		this(outFolder, name, Arrays.asList(keywords));
	}
	public String toString() {
		return name+"="+keywords.toString();
	}
	public KeywordOrganizer(Path outFolder, String name, List<String> keywords) throws IOException {
		this.keywords = new HashSet<String>();
		for (int i = 0; i < keywords.size(); i++) {
			String keyword = StringCleaner.cleanPhrase(keywords.get(i));
			if (!keyword.isEmpty()) {
				this.keywords.add(keyword);
			}
		}
		this.name = name;
		this.fw = new FileWriter(outFolder.resolve(name).toFile());
	}
	public boolean relevant(String commentText) {
		String comment = StringCleaner.cleanPhrase(commentText);
		if (commentText.isEmpty()) {
			return false;
		}
		for (String key : keywords) {
			if (comment.contains(key)) {
				return true;
			}
		}
		return false;
	}
	public void writeComment(String fullCommentJson) {
		try {
			fw.write(fullCommentJson + System.getProperty("line.separator"));
			fw.flush();
		} catch (IOException e) {
			System.err.println("Conflict: "+name+" failed to write");
			System.err.println(e.getMessage());
		}
	}
}
