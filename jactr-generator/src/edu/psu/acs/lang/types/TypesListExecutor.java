package edu.psu.acs.lang.types;

import edu.psu.acs.lang.lexsyn.LexsynOrderedList;
import util.sys.Executor;

public class TypesListExecutor extends Executor<TypesListProcessor, LexsynOrderedList, TypesList> {

	public TypesListExecutor() {
		super("typeslist", 5, TypesListProcessor.class, LexsynOrderedList.class, TypesList.class);
	}
	public static void main(String[] args) {
		TypesListExecutor tle = new TypesListExecutor();
		tle.initializeFromCmdLine(args);
		tle.run();
	}

}
