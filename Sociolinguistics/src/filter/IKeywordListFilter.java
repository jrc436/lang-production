package filter;

import util.data.Comment;

public interface IKeywordListFilter {
	public boolean keep(String key, Comment datum);
}
