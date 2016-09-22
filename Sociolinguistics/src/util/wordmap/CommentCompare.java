package util.wordmap;

import util.data.Comment;

@FunctionalInterface
public interface CommentCompare {
	public boolean inCorrectOrder(Comment first, Comment second);
}
