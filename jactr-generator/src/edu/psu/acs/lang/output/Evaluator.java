package edu.psu.acs.lang.output;

public interface Evaluator<E> {
	public String evalName();
	public String evaluate(E data);
}
