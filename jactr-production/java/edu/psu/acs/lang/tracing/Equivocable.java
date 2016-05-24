package edu.psu.acs.lang.tracing;

//equals refers to equal data in equivalent representation. However, for our use case (due to the recursion), we only really care about whether or not
//the two types are equivalent, not whether or not they are exactly equal. For instance, a different representation could cause it to fail equality.
//While it's possible that there is a deterministic way to determine equality while 
public interface Equivocable {
	/**
	 * Each override method should define the validators
	 * @param validators
	 * A set of objects that the function passes in to check if the object is funcitonally equivalent for some purpose
	 * @return
	 */
	public boolean validate(Object[] validators);
}
