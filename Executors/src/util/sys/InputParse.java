package util.sys;

public class InputParse {
	private final String inputDir;
	private final String outputDir;
	private final String[] remArgs;
	public InputParse(String[] args) {
		inputDir = args[0];
		outputDir = args[1];
		remArgs = new String[args.length-2];
		for (int i = 2; i < args.length; i++) {
			remArgs[i-2] = args[i];
		}
		
	}
	public String getInputPath() {
		return inputDir;
	}
	public String getOutputPath() {
		return outputDir;
	}
	public String[] getRemaining() {
		return remArgs;
	}
}
