package util.csv;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import filter.StringCleaner;

public class CSVReader {
	private final String[][] cells;
	public CSVReader(Path p, char commaRep) {
		List<String> lines = null;
		try {
			lines = Files.readAllLines(p);
		} catch (IOException e) {
			System.err.println("Failed to read from Path: "+p.toString());
			e.printStackTrace();
			System.exit(1);
		}
		cells = new String[lines.size()][lines.get(0).split(",").length];
		for (int i = 0; i < lines.size(); i++) {
			String[] lineCells = lines.get(i).split(",");
			for (int j = 0; j < lineCells.length; j++) {
				cells[i][j] = lineCells[j].replace(commaRep, ',');
			}	
		}
	}
	public String[][] getCleanedCells() {
		String[][] newCells = new String[cells.length][cells[0].length];
		for (int i = 0; i < cells.length; i++) {
			for (int j = 0; j < cells[0].length; j++) {
				newCells[i][j] = StringCleaner.sanitizeForFiles(cells[i][j]);
			}
		}
		return newCells;
	}
	public String[] getColumnByTitle(String title) {
		int titleVec = -1;
		for (int i = 0; i < cells[0].length; i++) {
			if (cells[0][i].contains(title)) {
				titleVec = i;
				break;
			}
		}
		if (titleVec == -1) {
			throw new IllegalArgumentException("Title: "+title+" not found in the CSV");
		}
		String[] vec = new String[cells.length-1];
		for (int i = 1; i < vec.length+1; i++) {
			vec[i-1] = cells[i][titleVec];
		}
		return vec;
	}
	
}
