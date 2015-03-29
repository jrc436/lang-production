package opennlp.ccg;

public enum ModelType {
	ACTR(),
	Ngram(),
	Random();
	public int getOrder() {
		switch (this) {
			case ACTR:
				return 4;
			case Ngram:
				return 4;
			default:
				return 0;
		}
	}
}
