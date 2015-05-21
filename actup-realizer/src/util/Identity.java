package util;


//for the use in identity hashmaps, map this object to the value
public class Identity<E> {
	private final E obj;
	public Identity(E obj) {
		this.obj = obj;
	}
	public int hashCode() {
		return System.identityHashCode(obj);
	}
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) {
			return false;
		}
		try {
			@SuppressWarnings("unchecked")
			Identity<E> id = (Identity<E>) o;
			return id.obj == this.obj;
		}
		catch (ClassCastException ce) {
			return false;
		}
	}
}
