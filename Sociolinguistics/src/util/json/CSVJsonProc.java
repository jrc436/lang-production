package util.json;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import util.csv.JsonCSV;
import util.sys.FileProcessor;

public class CSVJsonProc extends FileProcessor<JsonCSV, JsonList> {

	@Override
	public int getNumFixedArgs() {
		return 0;
	}

	@Override
	public String getConstructionErrorMsg() {
		return "No arguments need to be specified";
	}

	@Override
	public JsonCSV getNextData() {
		File f = super.getNextFile();
		if ( f == null) {
			return null;
		}
		try {
			BufferedReader bf = new BufferedReader(new FileReader(f));
			JsonCSV csv = null;
			int i = 0;
			while (true) {
				String line = bf.readLine();
				if (line == null) {
					break;
				}
				if (i == 0) {
					csv = new JsonCSV(line);
				}
				else {
					csv.addLine(line);
				}
			}
			bf.close();
			return csv;
		} catch (IOException e) {
			System.err.println("Error reading line from file: "+f.toString());
			e.printStackTrace();
			System.exit(1);
		}
		return null;
	}

	@Override
	public void map(JsonCSV newData, JsonList threadAggregate) {
		threadAggregate.addAll(newData);
	}

	@Override
	public void reduce(JsonList threadAggregate) {
		synchronized(processAggregate) {
			super.processAggregate.addAll(threadAggregate);
		}
	}

	@Override
	public boolean hasNArgs() {
		return false;
	}

}
