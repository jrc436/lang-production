package runconfig;

public class RealizationSettings {
	 protected static final int EDGE_LIMIT = Consts.MAX_EDGE_LIMIT;
	 protected static final int CELL_PRUNING_VALUE = Consts.PRUNE_LEAST_EDGES;
	 protected static final int EDGE_PRUNING_CUTOFF = Consts.PRUNE_LEAST_EDGES;  
	 protected static final boolean DO_UNPACKING = false; //only does something if also using packing    
	 protected static final boolean USE_COMBOS = true;   
	 protected static final boolean USE_PACKING = false;
	 protected static final int ITER_LIMIT = 15000;
	 protected static final boolean JOIN_FRAGMENTS = true;
	 protected static final boolean GLUE_FRAGMENTS = false;
	 
	 private int edgeLimit; //the maximum number of edges that can be created before the process terminates
	 private int cellPruningValue; //the maximum number of edges allowed in a cell - not sure what that means exactly
	 private int edgePruningValue; //the maximum number of edges allowed in the chart
	 private boolean doUnpacking; //requires usePacking to mean anything
	 private boolean useCombos; //not 100% on what this does
	 private boolean usePacking; //I'm not 100% on what this does. It seems like it and collectingCombos are opposites
	 private boolean joinFragments; //I'm not 100% on what this does. I assume it tries to join fragments by allowed rules
	 private boolean gluingFragments; //I'm not 100% on how this is different than joining fragments. I assume it just sticks two good fragments together.
	 private int iterLimit;
	  
	 protected RealizationSettings(int edgeLimit, int cellPruningValue, int edgePruningValue, boolean doUnpacking, boolean useCombos, boolean usePacking, boolean joinFragments, boolean glueFragments, int iterLimit) {
		 this.setEdgeLimit(edgeLimit);
		 this.setCellPruningValue(cellPruningValue);
		 this.setEdgePruningValue(edgePruningValue);
		 this.setDoUnpacking(doUnpacking);
		 this.setUseCombos(useCombos);
		 this.setUsePacking(usePacking);
		 this.setJoinFragments(joinFragments);
		 this.setGluingFragments(glueFragments);
		 this.setIterLimit(iterLimit);
	 }
	protected RealizationSettings() {
		this(EDGE_LIMIT, CELL_PRUNING_VALUE, EDGE_PRUNING_CUTOFF, DO_UNPACKING, USE_COMBOS, USE_PACKING, JOIN_FRAGMENTS, GLUE_FRAGMENTS, ITER_LIMIT);
	}
	public int getEdgeLimit() {
		return edgeLimit;
	}
	private void setEdgeLimit(int edgeLimit) {
		this.edgeLimit = edgeLimit;
	}
	public int getCellPruningValue() {
		return cellPruningValue;
	}
	private void setCellPruningValue(int cellPruningValue) {
		this.cellPruningValue = cellPruningValue;
	}
	public int getEdgePruningValue() {
		return edgePruningValue;
	}
	private void setEdgePruningValue(int edgePruningValue) {
		this.edgePruningValue = edgePruningValue;
	}
	public boolean doingUnpacking() {
		return doUnpacking && usePacking;
	}
	private void setDoUnpacking(boolean doUnpacking) {
		this.doUnpacking = doUnpacking;
	}
	public boolean collectingCombos() {
		return useCombos && !usePacking;
	}
	private void setUseCombos(boolean useCombos) {
		this.useCombos = useCombos;
	}
	public boolean usingPacking() {
		return usePacking;
	}
	private void setUsePacking(boolean usePacking) {
		this.usePacking = usePacking;
	}
	public boolean joiningFragments() {
		return joinFragments;
	}
	private void setJoinFragments(boolean joinFragments) {
		this.joinFragments = joinFragments;
	}
	public boolean gluingFragments() {
		return gluingFragments;
	}
	private void setGluingFragments(boolean gluingFragments) {
		this.gluingFragments = gluingFragments;
	}
	public int getIterLimit() {
		return iterLimit;
	}
	private void setIterLimit(int iterLimit) {
		this.iterLimit = iterLimit;
	}
}
