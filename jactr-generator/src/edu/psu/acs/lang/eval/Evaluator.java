package edu.psu.acs.lang.eval;

public interface Evaluator<E> {
	public String evalName();
	public String evaluate(E data);
}
