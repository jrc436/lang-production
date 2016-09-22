package util.csv;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.PriorityQueue;

import util.json.JsonList;
import util.json.JsonReadable;
import util.sys.DataType;

public class JsonCSV extends JsonList implements DataType {
	private static final long serialVersionUID = 4624711284907375813L;
	private final PriorityQueue<String> sortedKeys;
	public JsonCSV() {
		sortedKeys = new PriorityQueue<String>();
	}
	public JsonCSV(JsonCSV j) {
		this();
		for (JsonReadable jr : j) {
			this.add(jr);
		}
	}
	public JsonCSV(String headerLine) {
		this();
		String[] keys = headerLine.split(",");
		for (String key : keys) {
			sortedKeys.add(key);
		}
	}
	public void addLine(String line) {
		if (sortedKeys.isEmpty()) {
			throw new UnsupportedOperationException("This JsonCSV must be constructed first using the headerLine before it can be used this way.");
		}
		String[] values = line.split(",");
		JsonReadable jr = new JsonReadable();
		int i = 0;
		for (String key : sortedKeys) {
			jr.put(key, values[i]);
		}
		super.add(jr);
	}
	@Override
	public boolean add(JsonReadable jr) {
		if (sortedKeys.isEmpty()) {
			sortedKeys.addAll(jr.keySet());
		}
		return super.add(jr);
	}

	@Override
	public String getFileExt() {
		return ".csv";
	}
	@Override
	public Iterator<String> getStringIter() {
		Iterator<String> keys = this.sortedKeys.iterator();
		final JsonCSV outer = this;
		Iterator<String> iter = new Iterator<String>() {
			public boolean hasNext() {
				return keys.hasNext();
			}
			public String next() {
				String key = keys.next();
				String line = "";
				for (JsonReadable jr : outer) {
					line += jr.get(key) + ",";
				}
				return line.substring(0, line.length()-1);
			}
		};
		return iter;
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> lines = new ArrayList<String>();
		for (String key : sortedKeys) {
			String line = "";
			for (JsonReadable jr : this) {
				line += jr.get(key) + ",";
			}
			lines.add(line.substring(0, line.length()-1)); //trailing comma
		}
		return lines;
	}

	@Override
	public String getHeaderLine() {
		String line = "";
		for (String key : sortedKeys) {
			line += key + ",";
		}
		return line.substring(0, line.length()-1);
	}

	@Override
	public String getFooterLine() {
		return null;
	}

	@Override
	public DataType deepCopy() {
		return new JsonCSV(this);
	}

}
