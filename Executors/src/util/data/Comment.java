package util.data;

import java.time.Instant;

import util.json.JsonReadable;

/**
 * Just a json helper class
 * @author jrc436
 *
 */
public class Comment {
	private final String text;
	private final String author;
	private final Instant date;
	private final JsonReadable fields;
	public Comment(String text, String author, Instant date, JsonReadable json) {
		this.text = text;
		this.author = author;
		this.date = date;
		this.fields = json;
	}
	public String getText() {
		return text;
	}
	public boolean sameAuthor(Comment corp) {
		return corp.getAuthor().equals(this.getAuthor());
	}
	public boolean afterComment(Comment corp) {
		return this.date.isAfter(corp.getTime());
	}
	public String getAuthor() {
		return author;
	}
	public Instant getTime() {
		return date;
	}
	public String getField(String key) {
		return fields.get(key);
	}
	public JsonReadable copyDictionary() {
		return new JsonReadable(fields);
	}
	public String toString() {
		return fields.toString();
	}
}
