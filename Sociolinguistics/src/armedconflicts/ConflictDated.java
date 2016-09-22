package armedconflicts;

import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import filter.StringCleaner;
import util.csv.CSVReader;

@Deprecated
public class ConflictDated {
	private final HashMap<ConflictExternality, Integer[]> data;
	public ConflictDated() {
		this.data = new HashMap<ConflictExternality, Integer[]>();
	}
	public static Map<String, ConflictDated> getConflictDated(String pathToCSV, char commaRep) {
		CSVReader csv = new CSVReader(Paths.get(pathToCSV), commaRep);
		String[] names = csv.getColumnByTitle("Conflict");
		Map<String, ConflictDated> conflicts = new HashMap<String, ConflictDated>();
		for (int i = 0; i < names.length; i++) {
			String n = StringCleaner.sanitizeForFiles(names[i]);
			conflicts.put(n, new ConflictDated());
			names[i] = n;
		}
		
		for (int i = 2012; i < 2015; i++) {
			for (ConflictExternality c : ConflictExternality.values()) {
				String[] data = csv.getColumnByTitle(i+" "+c.toString());
				for (int j = 0; j < conflicts.size(); j++) {
					//conflicts should be in the same order as data and names
					if (data[j].equals("NULL")) {
						conflicts.get(names[j]).appendData(c, i, -1);
					}
					else {
						conflicts.get(names[j]).appendData(c, i, Integer.parseInt(StringCleaner.sanitizeForFiles(data[j])));
					}
				}
			}
		}
		return conflicts;
	}
	public int getDataForYear(ConflictExternality c, int year) {
		try {
			return data.get(c)[year-2012];
		}
		catch (ArrayIndexOutOfBoundsException e) {
			System.err.println("No data found for year: "+year);
			System.err.println(data.get(c));
			System.exit(1);
		}
		return -1;
	}
	private void appendData(ConflictExternality c, Integer year, Integer value) {
		if (!data.containsKey(c)) {
			data.put(c, new Integer[3]);
		}
		data.get(c)[year-2012] = value;
	}
}
enum ConflictExternality {
	Refugees,
	Fatalities,
	IDP;
}
