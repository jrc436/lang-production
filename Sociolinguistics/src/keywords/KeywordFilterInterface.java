package keywords;

import util.wordmap.WordMap;

public class KeywordFilterInterface extends KeywordMatchProcessor {
	public KeywordFilterInterface(String inpDir, String outDir, String[] cf, String[] fp) {
		super(inpDir, outDir, cf, getKeywords(fp[0]));
	}
	public KeywordFilterInterface() {
		super();
	}
	private static String[] getKeywords(String fp) {
		WordMap wm = WordMap.createFromDirectory(fp);
		String[] keywords = new String[wm.size()];
		int i = 0;
		for (String s : wm.keySet()) {
			keywords[i] = s;
			i++;
		}
		return keywords;
	}
}
