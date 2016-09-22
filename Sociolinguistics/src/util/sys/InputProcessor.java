package util.sys;

public class InputProcessor<J extends FileProcessor<K, V>, K extends DataType, V extends DataType> {
	private static final String boundDetection = "END";
	private final J dProc;
	private final K dInp;
	private final V dOut;
	private final InputParse ip;
	private final CmdLineInit[] cmd;
	public InputProcessor(Class<J> proc, Class<K> inp, Class<V> out, InputParse ip) {
		J dProc = null;
		K dInp = null;
		V dOut = null;
		try {
			dProc = proc.newInstance();
			dInp = inp.newInstance();
			dOut = out.newInstance();
		}
		catch (InstantiationException | IllegalAccessException e) {
			System.err.println("DataTypes and FileProcessors should have at least a dummy default constructor");
			e.printStackTrace();
			System.exit(1);
		}
		this.dProc = dProc;
		this.dInp = dInp;
		this.dOut = dOut;
		this.ip = ip;
		cmd = new CmdLineInit[] { dProc, dInp, dOut };
	}
	private boolean inputOverride() {
		return dProc.overrideInputArgs() != FileProcessor.DEFAULT;
	}
	private boolean outputOverride() {
		return dProc.overrideOutputArgs() != FileProcessor.DEFAULT;
	}
	public boolean indexIsBound(int ind) {
		return ip.getRemaining()[ind].equals(boundDetection);
	}
	public String[] getArgs(int start, int cmdI) {
		if (inputOverride() && cmd[cmdI] == dInp) {
			return processFixedArgs(start, start + dProc.overrideInputArgs());
		}
		if (outputOverride() && cmd[cmdI] == dOut) {
			return processFixedArgs(start, start + dProc.overrideOutputArgs());
		}
		String[] nargs = cmd[cmdI].hasNArgs() ? processNArgs(start) : new String[0];
		if (cmd[cmdI].hasNArgs()) {
			start += nargs.length+1;
		}
		String[] fargs = processFixedArgs(start, start+cmd[cmdI].getNumFixedArgs());
		String[] retval = new String[fargs.length + nargs.length];
		for (int i = 0; i < nargs.length; i++) {
			retval[i] = nargs[i];
		}
		for (int i = nargs.length; i < retval.length; i++) {
			retval[i] = fargs[i - nargs.length];
		}
		if (fargs.length != cmd[cmdI].getNumFixedArgs() || (cmd[cmdI].hasNArgs() && nargs.length == 0)) {
			createError();
		}
		return retval;
	}
	
	private String[] processFixedArgs(int start, int end) {
		if (end > ip.getRemaining().length) {
			System.err.println("Not enough fixed arguments were specified.");
			createError();
		}
		String[] retval = new String[end - start];
		for (int i = start; i < end; i++) {
			retval[i-start] = ip.getRemaining()[i]; 
		}
		return retval;
	}
	private String[] processNArgs(int start) {
		int end = start;
		for (int i = start; i < ip.getRemaining().length; i++) {
			if (!ip.getRemaining()[i].equals(boundDetection)) {
				end++;
			}
			else {
				//System.err.println("Bound detected");
				break;
			}
		}
		return processFixedArgs(start, end);
	}
	
	public void createError() {
		describeInput();
		if (outputOverride()) {
			System.err.println("OVERRIDE: output class ("+dOut.getClass().getName()+") requires exactly "+dProc.overrideOutputArgs()+" args");
		}
		if (inputOverride()) {
			System.err.println("OVERRIDE: input class ("+dInp.getClass().getName()+") requires exactly "+dProc.overrideInputArgs()+" args");
		}
		System.exit(1);
	}
	public void countErrorHandling(int[] counts, String[][] args) {
		for (int i = 0; i < counts.length; i++) {
			System.err.println(counts[i]+" were received for: "+cmd[i].getClass().getName()+". printing:");
			for (int j = 0; j < args[i].length; j++) {
				System.err.println(args[i][j]);
			}
		}
		System.err.println("Note that the count will sometimes be higher than the displayed args, if one was: "+boundDetection);
	}
	private void describeInput() {
		System.err.println("The first argument should be the input directory. The second should be the output directory.");
		for (int i = 0; i < cmd.length; i++) {
			int j = i +1;
			System.err.println("Set of arguments ("+j+") are for: "+cmd[i].getClass().getName());
			if (cmd[i].hasNArgs()) {
				System.err.println("First, please input the required NArgs, followed by: "+boundDetection);
			}
			System.err.println("After, input the fixed arguments, it takes: "+cmd[i].getNumFixedArgs()+" arguments");
			System.err.println("Construction Message: "+cmd[i].getConstructionErrorMsg());
			if (i != cmd.length-1 && cmd[i].getClass().equals(cmd[i+1].getClass())) {
				System.err.println("If arguments are NOT specified for: "+cmd[i+1].getClass().getName()+", system will attempt to repeat arguments");
			}
		}
	}
}
