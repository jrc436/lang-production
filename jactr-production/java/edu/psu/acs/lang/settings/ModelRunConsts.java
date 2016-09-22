package edu.psu.acs.lang.settings;

import edu.psu.acs.lang.hook.UtilityInitialization;

public class ModelRunConsts {
	public static final String outputFileBase = "output.mout";
	
	public static String getOutputPath(UtilityInitialization ui) {
		return ProcessConsts.getExpRunDir(ExperimentSettings.workingDir, ExperimentSettings.expVersion, ExperimentSettings.expName) + ui.toString()+"-"+outputFileBase;
	}
}
