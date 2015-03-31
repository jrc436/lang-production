package runconfig;

public enum EvaluationType {
	EditDistanceChar,
	EditDistanceWord,
	ROUGE;
	public String extPath() {
		switch (this) {
			case ROUGE:
				return Settings.basePath+"actup-production/rouge/";
			default:
				return "";
		}
	}
}
