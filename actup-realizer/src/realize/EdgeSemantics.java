package realize;

import java.util.BitSet;

import synsem.Sign;

public class EdgeSemantics {
	private final BitSet bits;
	private final Sign sign;
	public EdgeSemantics(BitSet bits, Sign sign) {
		this.bits = bits;
		this.sign = sign;
	}
	public boolean equals(Object other) {
		if (other.getClass() != this.getClass()) {
			return false;
		}
		EdgeSemantics o = (EdgeSemantics) other;
		return this.bits.equals(o.bits) && this.sign.getCategory().equalsNoLF(o.sign.getCategory());
	}
	public int hashCode() {
		return this.bits.hashCode() + this.sign.getCategory().hashCodeNoLF();
  }
}
