package runconfig;

public enum EvaluationType {
	EditDistanceChar,
	EditDistanceWord,
	ROUGE;
	public String extPath() {
		switch (this) {
			case ROUGE:
				return IOSettings.basePath+"actup-production/rouge/";
			default:
				return "";
		}
	}
}
