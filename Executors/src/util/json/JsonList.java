package util.json;

import java.util.ArrayList;
import java.util.Iterator;

import util.sys.DataType;
import util.sys.FileWritable;

public class JsonList extends ArrayList<JsonReadable> implements DataType {
	public JsonList() {
		super();
	}
	public JsonList(JsonList jl) {
		super(jl);
	}
	private static final long serialVersionUID = -429282291590763784L;

	@Override
	public String getFileExt() {
		return ".json";
	}

	@Override
	public int getNumFixedArgs() {
		return 0;
	}

	@Override
	public String getConstructionErrorMsg() {
		return "No further arguments should be given";
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		return JsonLayer.collectJsons(this);
	}

	@Override
	public DataType deepCopy() {
		return new JsonList(this);
	}

	@Override
	public String getHeaderLine() {
		return "[";
	}

	@Override
	public String getFooterLine() {
		return "]";
	}
//	@Override
//	public boolean isFull(int gbAllocated) {
//		return this.size() > 100000*gbAllocated;
//	}
	@Override
	public boolean hasNArgs() {
		// TODO Auto-generated method stub
		return false;
	}
	@Override
	public Iterator<String> getStringIter() {
		return FileWritable.<JsonReadable,JsonList>iterBuilder(this);
	}

}
