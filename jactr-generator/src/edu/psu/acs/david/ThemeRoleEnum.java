package edu.psu.acs.david;

import edu.psu.acs.lang.declarative.SlotValue;

/**
 * The way it's defined in act-up, each of "slot, marker, done, and lexform" are all separate. I will make them the same until I see
 * more compelling evidence they need to be the same
 * @author jrc
 *
 */
public enum ThemeRoleEnum implements SlotValue {
	theme,
	agent,
	goalrole,
	functor;
}
