package util.sys;

@FunctionalInterface
public interface Stringify<E> {
	public String makeString(E typ);
}
