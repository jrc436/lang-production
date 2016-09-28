package util.collections;

class OrderedPair<E, V> extends Pair<E, V> {
	protected OrderedPair(E typeOne, V typeTwo) {
		super(typeOne, typeTwo);
	}
	@Override
	public int hashCode() {
		return 23 * typeOne.hashCode() + 17 * typeTwo.hashCode();
	}
	@Override
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) {
			return false;
		}
		@SuppressWarnings("rawtypes")
		OrderedPair up = (OrderedPair)o;
		if (this.typeOne == null && this.typeTwo == null) {
			return up.typeOne == null && up.typeTwo == null;
		}
		else if (this.typeOne == null && up.typeOne == null) {
			return this.typeTwo.equals(up.typeTwo);
		}
		else if (this.typeTwo == null && up.typeTwo == null) {
			return this.typeOne.equals(up.typeOne);
		}
		else if (this.typeOne == null || this.typeTwo == null) {
			return false;
		}
		return this.typeOne.equals(up.typeOne) && this.typeTwo.equals(up.typeTwo);
	}
}
