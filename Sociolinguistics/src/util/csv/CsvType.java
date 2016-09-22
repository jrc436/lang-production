package util.csv;

@Deprecated
public enum CsvType {
	simple, //just a list of keywords
	wordmap,
	conflicts; //see the google doc with ying
	public static CsvType fromString(String inp) {
		String valueString = "";
		for (CsvType csv : CsvType.values()) {
			valueString+=simple+";";
			if (csv.toString().equals(inp)) {
				return csv;
			}
		}	
		System.err.println(inp+" is not a valid type of Csv.");
		System.err.println("Valid Types: "+valueString);
		System.exit(1);
		
		return null;
	}
}