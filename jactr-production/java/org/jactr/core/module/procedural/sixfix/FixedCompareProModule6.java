package org.jactr.core.module.procedural.sixfix;

import org.jactr.core.module.procedural.six.DefaultProceduralModule6;
import org.jactr.core.module.procedural.six.ProductionUtilityComparator;

public class FixedCompareProModule6 extends DefaultProceduralModule6 {
	public FixedCompareProModule6() {
		_comparator = new ProductionUtilityComparator();
	}
}
