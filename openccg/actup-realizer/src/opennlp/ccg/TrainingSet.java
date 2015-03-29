package opennlp.ccg;

public enum TrainingSet {
	SWBD,
	WSJ,
	NONE;
	public String getLMDirPath() {
		switch (this) {
			case SWBD:
				return Consts.experimentBasePath+"swbd-lm/";
			case WSJ:
				return Consts.experimentBasePath+"wsj-lm/";
			default:
				return "";
		}
	}
}
