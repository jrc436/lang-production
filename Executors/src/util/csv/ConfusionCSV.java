package util.csv;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;

import util.collections.DoubleKeyMap;
import util.collections.Pair;
import util.sys.DataType;

public class ConfusionCSV<K> extends DoubleKeyMap<K, K, Integer> implements DataType {
	public ConfusionCSV() {
		super();
	}
	public ConfusionCSV(boolean symm) {
		super(symm);
	}
	public ConfusionCSV(String[] symm) {
		super(Boolean.parseBoolean(symm[0]));
	}
	public ConfusionCSV(ConfusionCSV<K> csv) {
		for (Pair<K, K> p : csv.keySet()) {
			this.put(p, csv.get(p));
		}
	}
	private static final long serialVersionUID = -7806977361007415771L;

	@Override
	public int getNumFixedArgs() {
		return 1;
	}

	@Override
	public boolean hasNArgs() {
		return false;
	}
	
	public synchronized void absorb(ConfusionCSV<K> other) {
		for (Entry<Pair<K, K> , Integer> p : this.entrySet()) {
			if (other.containsKey(p.getKey())) {
				p.setValue(p.getValue()+other.get(p.getKey()));
			}
		}
		for (Entry<Pair<K, K>, Integer> p : other.entrySet()) {
			if (!this.containsKey(p.getKey())) {
				this.put(p.getKey(), p.getValue());
			}
		}
	}

	@Override
	public String getConstructionErrorMsg() {
		return "ConfusionCSV requires a true or false to whether the Confusion matrix is symmetric";
	}

	@Override
	public String getFileExt() {
		return ".csv";
	}

	@Override
	public ArrayList<String> getDataWriteLines() {
		ArrayList<String> lines = new ArrayList<String>();
		for (K key2 : super.getKeysetTwo()) {
			String line = key2.toString()+",";
			for (K key1 : super.getPairedKeys2(key2)) {
				line += super.get(key1, key2)+",";
			}
			line = line.substring(0, line.length()-1);
			lines.add(line);
		}
		return lines;
	}

	@Override
	public String getHeaderLine() {
//		String line = "";
//		for (K key : super.getKeysetOne()) {
//			line += key.toString() + ",";
//		}
//		return line.substring(0, line.length()-1);
		String line = "";
		Set<K> keyset = super.getKeysetOne();
		for (K key : keyset) {
			line += key.toString() + ",";
		}
		if (keyset.size() == 0) {
			System.err.println("Error: no elements found.");
			System.err.println("Collection should have: "+super.size()+" elements.");
		}
		return line.substring(0, line.length()-1);
	}

	@Override
	public String getFooterLine() {
		return null;
	}

	@Override
	public Iterator<String> getStringIter() {
		final ConfusionCSV<K> outer = this;
		Iterator<String> iter = new Iterator<String>() {
			Iterator<K> vertLabels = outer.getKeysetTwo().iterator();
			//Set<K> keysetOne = outer.getKeysetOne();
			public boolean hasNext() {
				return vertLabels.hasNext();
			}
			public String next() {
				K key2 = vertLabels.next();
				String line = key2.toString()+",";
				for (K key1 : outer.getPairedKeys2(key2)) {
					int shared = outer.containsKey(key1,key2) ? outer.get(key1, key2) : 0;
					line += shared + ",";
				}
				return line.substring(0, line.length()-1);
			}
		};
		return iter;
	}

	@Override
	public DataType deepCopy() {
		return new ConfusionCSV<K>(this);
	}

}
