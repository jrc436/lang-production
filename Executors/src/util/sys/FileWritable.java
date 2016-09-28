package util.sys;

import java.util.ArrayList;
import java.util.Iterator;

public interface FileWritable extends IterStrings {
	public String getFileExt();
	public ArrayList<String> getDataWriteLines();
	public String getHeaderLine();
	public String getFooterLine();
	public static <K, E extends Iterable<K>> Iterator<String> iterBuilder(E obj, Stringify<K> s) {
		Iterator<K> it = obj.iterator();
		Iterator<String> iter = new Iterator<String>() {
			public boolean hasNext() {
				return it.hasNext();
			}
			public String next() {
				K ob = it.next();
				return s.makeString(ob);
			}
		};
		return iter;
	}
	public static <K, E extends Iterable<K>> Iterator<String> iterBuilder(E obj) {
		return iterBuilder(obj, FileWritable::convertToString);
	}
	static <K> String convertToString(K type) {
		return type.toString();
	}
}
interface IterStrings {
	public Iterator<String> getStringIter();
}
