package util.wordmap;

import java.util.ArrayList;
import java.util.List;

import util.data.Comment;
import util.json.JsonReadable;

//too slow to be very useful, unfortunately !
@Deprecated 
public class JsonListCombine extends Combinable {
	private static final String separator = "$uvu$";
	private final List<JsonReadable> list;
	@SafeVarargs
	public JsonListCombine(JsonReadable...es) {
		this();
		for (JsonReadable el : es) {
			list.add(el);
		}
	}
	public JsonListCombine() {
		list = new ArrayList<JsonReadable>();
	}
	public JsonListCombine(List<JsonReadable> list) {
		this.list = list;
	}
	public JsonListCombine(String s) {
		this();
		String[] strings = s.split(separator);
		for (String f : strings) {
			list.add(JsonReadable.fromString(f));
		}
	}
	public JsonListCombine(Comment c) {
		this();
		list.add(c.copyDictionary());
	}
	public String toString() {
		String toReturn = "";
		for (JsonReadable s : list) {
			toReturn += s.toString() + separator;
		}
		return toReturn.substring(0, toReturn.length()-separator.length());
	}
	@Override
	public Combinable combine(Combinable other) throws CombineException {
		if (!(other instanceof JsonListCombine)) {
			throw new CombineException();
		}
		JsonListCombine edc = (JsonListCombine) other;
		List<JsonReadable> newList = new ArrayList<JsonReadable>();
		newList.addAll(edc.list);
		newList.addAll(this.list);
		return new JsonListCombine(newList);
	}

}
