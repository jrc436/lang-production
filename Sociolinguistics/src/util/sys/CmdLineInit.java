package util.sys;

public interface CmdLineInit {
	/**
	 * Gets the required number of command line arguments to create this class. Fixed Args always occur after NArgs,
	 * if both are present
	 * @return
	 * The number of command line args required, excluding NArgs
	 */
	public int getNumFixedArgs();
	/**
	 * 
	 * @return
	 */
	public boolean hasNArgs();
	public String getConstructionErrorMsg();
}
