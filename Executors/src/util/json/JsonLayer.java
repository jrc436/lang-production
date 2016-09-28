package util.json;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;

import util.data.Comment;
import util.data.CommentFormat;
import util.sys.DataType;
import util.sys.FileProcessor;

public abstract class JsonLayer<K extends DataType> extends FileProcessor<JsonList, K> {
	private final CommentFormat cf;
	public JsonLayer(String inpDir, String outDir, K aggregate, String commentFormat) {
		super(inpDir, outDir, aggregate);
		this.cf = CommentFormat.fromString(commentFormat);
	}
	protected JsonLayer() {
		super();
		this.cf = null;
	}
	public Comment getAsComment(JsonReadable jr) {
		return cf.getComment(jr);
	}
	@Override
	public int getNumFixedArgs() {
		return 1;
	}
	@Override
	public boolean hasNArgs() {
		return false;
	}
	@Override
	public String getConstructionErrorMsg() {
		return "All JsonLayers must specify the comment format of the incoming data";
	}
	public static JsonList getReadable(File f) {
		JsonList allMessages = new JsonList();
		FileReader fr = null;
		try {
			fr = new FileReader(f);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			System.err.println("Skipping file that's supposed to be at: "+f.getAbsolutePath());
			return allMessages;
		}
		JsonArray ja = null;
		try {
			ja = Json.createReader(fr).readArray();
			//System.out.println("Reading JsonArray finished");
			Iterator<JsonValue> it = ja.iterator();
			while (it.hasNext()) {
				JsonReadable message = new JsonReadable();
				JsonValue jv = it.next();
				try {
					JsonObject jo = (JsonObject) jv;
					for (String key : jo.keySet()) {
						message.put(key, jo.get(key).toString());
					}
					allMessages.add(message);
				}
				catch (Exception e) {
					System.err.println(e);
					System.err.println(jv.getClass());
					System.exit(1);
				}
			}
		}
		catch (javax.json.stream.JsonParsingException jpe) {
			System.err.println(f.toPath());
			System.err.println(jpe.getMessage());
			System.exit(1);
		}
	//	System.out.println("Transformed into Map finished");
		return allMessages;
	}
//	public JsonList getReadableByName(String f) {
//		File r = null;
//		for (File s : super.files) {
//			if (s.getName().contains(f)) {
//				r = s;
//				break;
//			}
//		}		
//		return r == null ? null : getReadable(r);
//	}
	public JsonList getNextData() {	
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		return getReadable(f);	
	}
	public static ArrayList<String> collectJsons(Collection<JsonReadable> t) {
		ArrayList<String> retval = new ArrayList<String>();
		for (JsonReadable jr : t) {
			retval.add(jr.toString());
		}
		return retval;
	}
	public static String collectJsons(Collection<JsonReadable> transformation, boolean fullFile) {
		String ret = "";
		if (fullFile) {
			ret += "["+System.getProperty("line.separator");
		}
		int i = 0;
		for (JsonReadable jr : transformation) {
			ret += jr.toString();
			if (i != transformation.size() - 1) {
				ret += ","+System.getProperty("line.separator");
			}
			else if (fullFile) { //it is the last line, and it is fullFile
				ret+=System.getProperty("line.separator")+"]";
			}
			i++;
		}
		return ret;
	}
}
