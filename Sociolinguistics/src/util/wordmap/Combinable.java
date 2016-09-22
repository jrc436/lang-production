package util.wordmap;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import util.data.Comment;

public abstract class Combinable {
	public abstract Combinable combine(Combinable other) throws CombineException;
	public String clsString() {
		return this.getClass().getName();
	}
	
	@SuppressWarnings("unchecked")
	public static Set<Class<? extends Combinable>> fromString(String header) {
		Set<Class<? extends Combinable>> set = new HashSet<Class<? extends Combinable>>();
		if (header.contains(CombineSet.clsSplit)) {
			String[] types = header.split(CombineSet.clsSplit);
			for (String type : types) {
				if (!type.isEmpty()) {
					try {
						set.add((Class<? extends Combinable>) Class.forName(type));
					} catch (ClassNotFoundException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
						System.err.println("Class: "+type+" was either not found or doesn't implement Combinable. This header is illegal.");
						System.err.println("fatal error");
						System.exit(1);
					}
				}
			}
		}
		else {
			try {
				set.add((Class<? extends Combinable>) Class.forName(header));
			} catch (ClassNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
				System.err.println("Class: "+header+" was either not found or doesn't implement Combinable. This header is illegal.");
				System.err.println("fatal error");
				System.exit(1);
			}
		}
		return set;
	}
	public static Combinable recreate(Set<Class<? extends Combinable>> combinables, String s) {
		if (combinables.size() == 0) {
			return null;
		}
		else if (combinables.size() == 1) {
			Class<? extends Combinable> c = combinables.iterator().next();
			try {
				return c.getConstructor(String.class).newInstance(s);
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				System.err.println("Error with class: "+c.toString());
				System.err.println("This is likely due to the nonexistence of a public constructor using a String");
				System.err.println("This is a fatal error.");
				e.printStackTrace();
				System.exit(1);
			}
		}
		return new CombineSet(combinables, s);
	}
	public static Combinable initialize(Set<Class<? extends Combinable>> combinables) {
		if (combinables.size() == 0) {
			return null;
		}
		else if (combinables.size() == 1) {
			Class<? extends Combinable> c = combinables.iterator().next();
			try {
				return c.newInstance();
			} catch (InstantiationException | IllegalAccessException e) {
				System.err.println("Error with class: "+c.toString());
				System.err.println("This is likely due to the nonexistence of a public default constructor");
				System.err.println("This is a fatal error.");
				e.printStackTrace();
				System.exit(1);
			}
		}
		Map<Class<? extends Combinable>, Combinable> elements = new HashMap<Class<? extends Combinable>, Combinable>();
		for (Class<? extends Combinable> cls : combinables) {
			try {
				elements.put(cls, cls.newInstance());
			} catch (InstantiationException | IllegalAccessException e) {
				System.err.println("Error with class: "+cls.toString());
				System.err.println("This is likely due to the nonexistence of a public default constructor");
				System.err.println("This is a fatal error.");
				e.printStackTrace();
				System.exit(1);
			}
		}
		return new CombineSet(elements);
	}
	
	public static Combinable populate(Set<Class<? extends Combinable>> combinables, Comment data) {
		if (combinables.size() == 0) {
			return null;
		}
		else if (combinables.size() == 1) {
			Class<? extends Combinable> c = combinables.iterator().next();
			try {
				return c.getConstructor(Comment.class).newInstance(data);
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				System.err.println("Error with class: "+c.toString());
				System.err.println("This is likely due to the nonexistence of a public constructor using a Comment");
				System.err.println("This is a fatal error.");
				e.printStackTrace();
				System.exit(1);
			}
		}
		Map<Class<? extends Combinable>, Combinable> elements = new HashMap<Class<? extends Combinable>, Combinable>();
		for (Class<? extends Combinable> cls : combinables) {
			try {
				elements.put(cls, cls.getConstructor(Comment.class).newInstance(data));
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				System.err.println("Error with class: "+cls.toString());
				System.err.println("This is likely due to the nonexistence of a public constructor using a Comment");
				System.err.println("This is a fatal error.");
				e.printStackTrace();
				System.exit(1);
			}
		}
		return new CombineSet(elements);
	}
}
