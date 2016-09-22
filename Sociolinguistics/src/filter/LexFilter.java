package filter;

import java.util.Map;
import java.util.regex.Pattern;

import util.wordmap.Combinable;
import util.wordmap.WordMap;

public class LexFilter extends WordMapFilter {
	private static final Pattern wordDef = Pattern.compile("^[a-zA-Z]+$");
	private static boolean isWord(Map.Entry<String, Combinable> s) {
		return wordDef.matcher(s.getKey()).matches();
	}
	@Override
	protected IWordMapFilter createFilter(WordMap wm) {
		return LexFilter::isWord;
	}
}
