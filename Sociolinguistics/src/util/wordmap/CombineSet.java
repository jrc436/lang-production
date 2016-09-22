package util.wordmap;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * In practice this doesn't work so well, because the default constructor can't know which to initialize
 * @author jrc436
 *
 */
public class CombineSet extends Combinable {
	private final Map<Class<? extends Combinable>, Combinable> elements;
	public final static String clsSplit = ";;::;;";
	public CombineSet(Combinable...combinables) {
		this();
		for (Combinable c : combinables) {
			elements.put(c.getClass(), c);
		}
	}
	
	public CombineSet() {
		elements = new HashMap<Class<? extends Combinable>, Combinable>();
	}
	public CombineSet(Map<Class<? extends Combinable>, Combinable> elements) {
		this.elements = elements;
	}
	public CombineSet(Set<Class<? extends Combinable>> combinables, String s) {
		this();
		String[] entries = s.split(entrySplit);
		for (String entry : entries) {
			String[] pair = entry.split(clsEntrySplit);	
			try {
				@SuppressWarnings("unchecked")
				Class<? extends Combinable> cls = (Class<? extends Combinable>) Class.forName(pair[0]);
				elements.put(cls, cls.getConstructor(String.class).newInstance(pair[1]));
			} catch (ClassNotFoundException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				e.printStackTrace();
				System.exit(1);
			}
		}
		for (Class<? extends Combinable> cls : this.elements.keySet()) {
			if (!combinables.contains(cls)) {
				throw new IllegalArgumentException("String mismatches with set in creating CombineSet");
			}
		}
		for (Class<? extends Combinable> cls : combinables) {
			if (!this.elements.containsKey(cls)) {
				throw new IllegalArgumentException("String mismatches with set in creating CombineSet");
			}
		}
	}
	public Combinable get(Class<? extends Combinable> comb) {
		return elements.get(comb);
	}
	public void set(Class<? extends Combinable> comb, Combinable c) {
		if (elements.containsKey(comb)) {
			elements.put(comb, c);
		}
	}
	public String clsString() {
		String head = "";
		for (Class<? extends Combinable> cls : elements.keySet()) {
			head += cls.getName()+clsSplit;
		}
		return head;
	}
	private static final String entrySplit = "%--%";
	private static final String clsEntrySplit = "&:&";
	public String toString() {
		String agg = "";
		for (Class<? extends Combinable> cls : elements.keySet()) {
			agg += elements.get(cls).clsString()+clsEntrySplit+elements.get(cls).toString()+entrySplit;
		}
		agg.substring(0, agg.length()-entrySplit.length());
		return agg;
	}
	public static String mockClsString(Set<Class<? extends Combinable>> el) {
		Map<Class<? extends Combinable>, Combinable> map = new HashMap<Class<? extends Combinable>, Combinable>();
		for (Class<? extends Combinable> c : el) {
			map.put(c, null);
		}
		return new CombineSet(map).clsString();
	}

	@Override
	public Combinable combine(Combinable other) throws CombineException {
		if (!(other instanceof CombineSet)) {
			throw new CombineException();
		}
		CombineSet otm = (CombineSet) other;
		for (Class<? extends Combinable> cls : this.elements.keySet()) {
			if (!otm.elements.containsKey(cls)) {
				throw new CombineException();
			}
		}
		for (Class<? extends Combinable> cls : otm.elements.keySet()) {
			if (!this.elements.containsKey(cls)) {
				throw new CombineException();
			}
		}
		Map<Class<? extends Combinable>, Combinable> newEl = new HashMap<Class<? extends Combinable>, Combinable>();
		for (Class<? extends Combinable> cls : this.elements.keySet()) {
			newEl.put(cls, this.elements.get(cls).combine(otm.elements.get(cls)));
		}
		return new CombineSet(newEl);
	}
}
