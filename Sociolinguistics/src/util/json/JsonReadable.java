package util.json;

import java.util.HashMap;

public class JsonReadable extends HashMap<String, String> {
	/**
	 * 
	 */
	private static final long serialVersionUID = -6456148281092346639L;
	public JsonReadable() {
		super();
	}
	public JsonReadable(JsonReadable jsonReadable) {
		super(jsonReadable);
	}
	@Override
	public String put(String key, String value) {	
		return super.put(key, stripQuotes(value));
	}
	private static String stripQuotes(String value) {
		if (value.charAt(0) == '\"' && value.charAt(value.length()-1) == '\"') {
			return value.substring(1, value.length()-1);
		}
		return value;
	}
	private static String stripBrackets(String value) {
		if (value.charAt(0) == '{' && value.charAt(value.length()-1) == '}') {
			return value.substring(1, value.length()-1);
		}
		return value;
	}
	public static JsonReadable fromString(String fromString) {
		if (fromString.charAt(0) != '{' || fromString.charAt(fromString.length()-1) != '}') {
			System.err.println(fromString);
			throw new IllegalArgumentException(fromString+ " does not appear to be a valid JSON");
		}
		String noBracks = stripBrackets(fromString);
		String[] allKeys = noBracks.split(",");
		JsonReadable toReturn = new JsonReadable();
		for (String s : allKeys) {
			String[] keyval = s.split(":");
			if (keyval.length != 2) {
				System.err.println("Formatting problem with colons:");
				System.err.println(fromString);
				System.err.println(s);
				System.exit(1);
			}
			toReturn.put(keyval[0], keyval[1]);
		}
		return toReturn;
	}
	public String toString() {
		String ret = "";
		ret += "{";
		for (Entry<String, String> entries : this.entrySet()) {
			ret += "\""+entries.getKey()+"\"";
			ret += ":";
			//ret += "\"" + entries.getValue() + "\"";
			ret += entries.getValue();
			ret += ",";
		}
		ret = ret.substring(0, ret.length()-1);
		ret += "}";
		return ret;
	}
}
