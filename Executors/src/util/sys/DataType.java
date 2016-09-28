package util.sys;

public interface DataType extends CmdLineInit, FileWritable {
	public DataType deepCopy();
	/**
	 * This is a subjective method where a class has to declare if it's "full". Full is defined as having taken up
	 * enough memory that it should be written out. This is subjective because the amount of memory declared is thus
	 * ambiguous. A simple implementation can just be to always return false, unless memory requirements of the task are
	 * very high
	 * @return
	 * false, or a metric for determining when a thread should forcibly call reduce
	 */
	//public boolean isFull(int gbAllocated);
	//public DataType fromFile(File f);
}
