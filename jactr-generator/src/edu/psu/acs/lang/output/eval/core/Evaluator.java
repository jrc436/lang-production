package edu.psu.acs.lang.output.eval.core;

public interface Evaluator<E> {
	public String evalName();
	public String evaluate(E data);
}
