package util.data;

import java.time.Instant;

import util.json.JsonReadable;

public enum CommentFormat {
	reddit;
	public Comment getComment(JsonReadable jr) {
		switch(this) {
			case reddit:
				return new Comment(jr.get("body"), jr.get("author"), Instant.ofEpochSecond(Long.parseLong(jr.get("created_utc"))), jr);
			default:
				throw new UnsupportedOperationException("Please implement this type of format");
		}
	}
	public static CommentFormat fromString(String s) {
		String err = "valid values:";
		for (CommentFormat cf : CommentFormat.values()) {
			if (cf.toString().equals(s)) {
				return cf;
			}
			err += cf.toString()+",";
		}
		err += "Received: "+s;
		throw new IllegalArgumentException(err);
	}
}
