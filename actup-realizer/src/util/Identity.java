package util;


//for the use in identity hashmaps, map this object to the value
public class Identity {
	private final Object obj;
	public Identity(Object obj) {
		this.obj = obj;
	}
	public int hashCode() {
		return System.identityHashCode(obj);
	}
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) {
			return false;
		}
		Identity id = (Identity) o;
		return id.obj == obj;
	}
}
