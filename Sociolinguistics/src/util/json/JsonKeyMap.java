package util.json;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.NoSuchElementException;

import util.sys.DataType;

public class JsonKeyMap extends HashMap<String, JsonList> implements DataType {

	private static final long serialVersionUID = 1L;
	private final String primaryKey;
	public JsonKeyMap(String primaryKey) {
		this.primaryKey = primaryKey;
	}
	public JsonKeyMap(String primaryKey, JsonList jl) {
		this(primaryKey);
		addAll(jl);
	}
	public void appendMap(JsonKeyMap jkm) {
		for (String primKey : jkm.keySet()) {
			if (this.keySet().contains(primKey)) {
				this.get(primKey).addAll(jkm.get(primKey));
			}
		}
	}
	public void mergeMapJsons(JsonKeyMap jkm) {
		for (String primKey : jkm.keySet()) {
			if (this.keySet().contains(primKey)) {
				JsonList other = jkm.get(primKey);
				JsonList ours = this.get(primKey);
				for (JsonReadable jr1 : ours) {
					for (JsonReadable jr2 : other) {
						mergeJson(jr1, jr2);
					}
				}
			}
		}
	}
	private void mergeJson(JsonReadable mergeInto, JsonReadable takeData) {
		for (String s : takeData.keySet()) {
			if (mergeInto.containsKey(s) && !mergeInto.get(s).equals(takeData.get(s))) {
				throw new IllegalArgumentException("Incompatible Json Streams are causing unpredictable results");
			}
			mergeInto.put(s, takeData.get(s));
		}
	}
	public void add(JsonReadable jr) {
		if (!this.containsKey(jr.get(primaryKey))) {
			this.put(jr.get(primaryKey), new JsonList());
		}
		this.get(jr.get(primaryKey)).add(jr);
	}
	public void addAll(JsonList jl) {
		for (JsonReadable jr : jl) {
			this.add(jr);
		}
	}
	
	@Override
	public int getNumFixedArgs() {
		return 1;
	}

	@Override
	public String getConstructionErrorMsg() {
		return "The primary key of the data must be specified";
	}

	@Override
	public String getFileExt() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		JsonList full = new JsonList();
		for (String s : this.keySet()) {
			full.addAll(this.get(s));
		}
		return JsonLayer.collectJsons(full);
	}
	
	@Override
	public Iterator<String> getStringIter() {
		Iterator<String> keys = this.keySet().iterator();
		final JsonKeyMap outer = this;
		Iterator<String> iter = new Iterator<String>() {
			private Iterator<JsonReadable> vals;
			
			public boolean hasNext() {
				return keys.hasNext() || (vals != null && vals.hasNext());
			}
			public String next() {
				if (vals == null) {
					vals = outer.get(keys.next()).iterator();
				}
				if (vals.hasNext()) {
					return vals.next().toString();
				}
				else {
					vals = null;
				}
				throw new NoSuchElementException();
			}
		};
		return iter;
	}

	@Override
	public String getHeaderLine() {
		return "[";
	}

	@Override
	public String getFooterLine() {
		return "]";
	}

	@Override
	public DataType deepCopy() {
		JsonKeyMap jkm = new JsonKeyMap(this.primaryKey);
		for (String s : this.keySet()) {
			jkm.put(s, (JsonList) this.get(s).deepCopy());
		}
		return jkm;
	}
//	@Override
//	public boolean isFull(int gbAllocated) {
//		return this.size() > 100*gbAllocated;
//	}
	@Override
	public boolean hasNArgs() {
		// TODO Auto-generated method stub
		return false;
	}

}
