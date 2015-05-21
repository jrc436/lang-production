package unify;

@FunctionalInterface 
public interface MutableScript {
	//takes a mutable, doesn't return anything
	public void run(Mutable m);
}