package util.wordmap;

import util.data.Comment;

@Deprecated
@FunctionalInterface
public interface InitWord {
	public Combinable initialValue(Comment data);
}
