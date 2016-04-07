package org.jactr.core.module.procedural.sixtrace;

import org.jactr.core.module.procedural.six.DefaultProceduralModule6;

public class TracerProceduralModule6 extends DefaultProceduralModule6 {
	public TracerProceduralModule6() {
		super();
		this.setProductionSelector(new TracerProductionSelector());
	}
}
