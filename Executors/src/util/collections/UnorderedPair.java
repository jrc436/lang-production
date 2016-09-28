package util.collections;

class UnorderedPair<E, V> extends Pair<E, V> {
	protected UnorderedPair(E typeOne, V typeTwo) {
		super(typeOne, typeTwo);
	}
	@Override
	public int hashCode() {
		return 23 * typeOne.hashCode() * typeTwo.hashCode();
	}
	@Override
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) {
			return false;
		}
		@SuppressWarnings("rawtypes")
		UnorderedPair up = (UnorderedPair)o;
		if (this.typeOne == null && this.typeTwo == null) {
			return up.typeOne == null && up.typeTwo == null;
		}
		else if (this.typeOne == null && up.typeOne == null) {
			return this.typeTwo.equals(up.typeTwo);
		}
		else if (this.typeOne == null && up.typeTwo == null) {
			return this.typeTwo.equals(up.typeOne);
		}
		else if (this.typeTwo == null && up.typeOne == null) {
			return this.typeOne.equals(up.typeTwo);
		}
		else if (this.typeTwo == null && up.typeTwo == null) {
			return this.typeOne.equals(up.typeOne);
		}
		else if (this.typeOne == null || this.typeTwo == null) {
			return false;
		}
		if (this.typeOne.equals(up.typeOne) && this.typeTwo.equals(up.typeTwo)) {
			return true;
		}
		return this.typeOne.equals(up.typeTwo) && this.typeTwo.equals(up.typeOne);
	}
}
