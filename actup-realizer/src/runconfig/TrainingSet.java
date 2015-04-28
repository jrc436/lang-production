package runconfig;

public enum TrainingSet {
	SWBDM1,
	SWBD10FOLD,
	WSJ,
	NONE;
	public String getLMDirPath() {
		switch (this) {
			case SWBDM1:
				return Consts.experimentBasePath+"swbd-m1-lm/";
			case SWBD10FOLD:
				return Consts.experimentBasePath+"swbd-10fold-lm";
			case WSJ:
				return Consts.experimentBasePath+"wsj-lm/";
			default:
				return "";
		}
	}
}
