package realize;

import java.util.BitSet;

import synsem.Sign;

public class EdgeSurfaceWords {
	  /** 
     * Returns whether this edge equals the given object based on the surface words, 
     * ignoring the LF and ignoring the score. 
     */
	private final BitSet bitset;
	private final BitSet indices;
	private final Sign sign;
	public EdgeSurfaceWords(BitSet bitset, BitSet indices, Sign sign) {
		this.bitset = bitset;
		this.indices = indices;
		this.sign = sign;
	}
	public int hashCode() {
		return this.surfaceWordHashCode();
	}
	public boolean equals(Object obj) {
		return this.surfaceWordEquals(obj);
	}
	private boolean surfaceWordEquals(Object obj) {
        if (obj.getClass() != this.getClass()) {
        	return false;
        }
        EdgeSurfaceWords esw = (EdgeSurfaceWords) obj;
        return bitset.equals(esw.bitset) && indices.equals(esw.indices) && sign.surfaceWordEquals(esw.sign, true);
    }
    /** 
     * Returns a hash code for this edge based on the surface words, ignoring the LF and ignoring the score. 
     */
    private int surfaceWordHashCode() {
    	return sign.surfaceWordHashCode(true) + 31 * bitset.hashCode() + indices.hashCode();
    }
}
