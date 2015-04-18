package optimization;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Random;

//a variable to be used for simple hill climbing
public class Variable {
	private double upperBound;
	protected double getUpper() {
		return upperBound;
	}
	private double increment;
	protected double getIncrement() {
		return increment;
	}
	private double lowerBound;
	protected double getLower() {
		return lowerBound;
	}
	private BigDecimal currentValue;
	private double lastValue;
	private boolean settledP; //no more improvements in the P direction
	private boolean settledN; //no more improvements in the N direction
	private double direction; //1 means moving in the pos direction, -1 means moving in the negative direction
	private boolean firstRun;
	private final String name;
	public Variable(String name, double initialValue, double lowerBound, double upperBound, double increment) {
		this.name = name;
		this.upperBound = upperBound;
		this.increment = increment;
		this.lowerBound = lowerBound;
		setCurrentValue(initialValue);
		this.lastValue = 0.0;
		this.firstRun = true;
		this.settledP = false;
		this.settledN = false;
		this.direction = new Random().nextBoolean() ? 1.0 : -1.0;
	}
	public String toString() {
		String retval = "";
		if (name != null) {
			retval += this.name+": ";
		}
		retval+=this.currentValue+";";
		return retval;
	}
	public Variable(double initialValue, double lowerBound, double upperBound, double increment) {
		this(null, initialValue, lowerBound, upperBound, increment);
	}
	public double getCurrentValue() {
		return this.currentValue.doubleValue();
	}
	//should probably never be made public. Consider making forceCurrentValue public if needed.
	private void setCurrentValue(double val) {
		this.currentValue = new BigDecimal(val);
		this.currentValue.setScale(4, RoundingMode.HALF_UP);
	}
	private void forceCurrentValue(double newValue) {
		//we want to "snap" the currentValue to something that's a valid increment to reduce the search space
		double newV = this.lowerBound;
		while (newValue > newV) {
			newV += this.increment;
		}
		//now newValue is between newV - increment and newV, so check which it's closer to
		if (newValue - (newV - increment) < newValue - newV) {
			//closer to newV - increment
			setCurrentValue(newV - increment);
		}
		else {
			setCurrentValue(newV);
		}
		//lastvalue is made irrelevant by this, so
		this.firstRun = true;
	}
	protected void resetValueRandom() {
		this.forceCurrentValue(new Random().nextDouble() * (this.upperBound - this.lowerBound) + this.lowerBound);
	}
	//if lastvalue is not set, then goodincr doesn't matter, we'll use whatever direction (typo caught by jamie) was initially set to
	//there are two reasons for the "first run" parameter. One is that if it is the true first run of the variable, there is no previous value.
	//This can be because every variable is still its initial value (very first run), or because step was called from updateindex
	public boolean step(boolean goodIncr) {
		if (this.firstRun) {
			this.firstRun = false;
			increment();
			return checkBounds();
		}
		//if the increment is good, do it again!
		if (goodIncr) {
			//if it's good, then we can "settle" the other way now, to be simple. It's possible (but unlikely) that we're in a minima,
			//which would make this strategy not work.
			settleOppositeDirection();
			//now that we've settled it, we should perform the increment again
			increment();
			return checkBounds();
		}		
		else {
			//reset since we had a decrease
			setCurrentValue(this.lastValue);
			settleCurrentDirection();
			if (settled()) {
				return false;
			}		
			//otherwise, changedirection and keep going		
			changeDirection();
			increment();
			return checkBounds();
		}
	}
	private void settleOppositeDirection() {
		changeDirection();
		settleCurrentDirection();
		changeDirection();
	}
	private void settleCurrentDirection() {
		if (direction < 0.0) {
			settledN = true;
		}
		else {
			settledP = true;
		}
	}
	private boolean settled() {
		return settledN && settledP;
	}
	//to be called after optimizing every other parameter to see if we can get further improvements
	public void unsettle() {
		settledN = false;
		settledP = false;
		//see checkbounds explanation
		if (!inBounds(this.getCurrentValue() + (direction * increment))) {
			changeDirection();
		}
		this.firstRun = true; //this is after a superiter... so we'll increment it right away so we don't run the same thing twice	
	}
	//after checkbounds returns true, settled should also return true in either case... (in one case, we had settled the other side because we were advancing,
	//in the other case, we settled the other side because it caused a bad incr.
	//the only time this isn't true is if it settles on the maximum, and unsettle is called to try to gain further improvements
	//this case will be handled in unsettle
	//if this is called in the first run (which should generally always return true), it will always return true because it's impossible that either
	//settledP or settledN is true yet, though one could be set to be true if it's at the max
	private boolean checkBounds() {
		if (this.getCurrentValue() > this.upperBound && direction > 0.0) {
			setCurrentValue(this.lastValue);
			settledP = true;
			if (!this.settled()) {
				changeDirection();
				return true;
			}
			return false;
		}
		else if (this.getCurrentValue() < this.lowerBound && direction < 0.0) {
			setCurrentValue(this.lastValue);
			settledN = true;
			if (!this.settled()) {
				changeDirection();
				return true;
			}
			return false;
		}
		return true;
	}
	private boolean inBounds(double val) {
		return val <= this.upperBound && val >= this.lowerBound;
	}
	private void increment() {
		this.lastValue = getCurrentValue();
		setCurrentValue(getCurrentValue() + increment * direction);
	}
	private void changeDirection() {
		if (this.direction < 0.0) {
			this.direction = 1.0;
		}
		else {
			this.direction = -1.0;
		}
	}
	
	public int hashCode() {
		return (int) (Math.pow(29, 1) * getCurrentValue()) + (int) (Math.pow(29, 2) * this.increment) + (int) (Math.pow(29,  3) * this.upperBound) + (int) (Math.pow(29, 4) * this.lowerBound);
	}
	public boolean equals(Object o) {
		if (o == null || o.getClass() != this.getClass()) { return false; }
		Variable v = (Variable) o;
		return this.currentValue == v.currentValue && this.increment == v.increment && this.upperBound == v.upperBound && this.lowerBound == v.lowerBound;
	}
}

