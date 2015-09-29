package edu.psu.acs.lang.declarative;

/**
 * These are the things in little brackets. I don't exactly understand them quite yet.
 * @author jrc
 *
 */
public enum CCGTypeModifier {
	frg,
	qem,
	ng,
	to,
	b,
	inv,
	intj,
	adj,
	thr,
	dcl,
	fr,
	pt,
	expl,
	as,
	bem,
	pss,
	em,
	nb,
	q,
	poss,
	wq,
	asup;
	public String toString() {
		switch (this) {
			case fr:
				return "for";
			default:
				return super.toString();
		
		}
	}
	public static CCGTypeModifier value(String s) {
		if (s.equals("for")) {
			return CCGTypeModifier.fr;
		}
		return valueOf(s);
	}
}
