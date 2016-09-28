package util.csv;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;

public class CSVWriter {
	private final String[][] cells;
	public CSVWriter(String[][] cells) {
		this.cells = cells;
	}
	public void writeCSV(Path p) throws IOException {
		FileWriter fw = new FileWriter(p.toFile());
		for (int i = 0; i < cells.length; i++) {
			String line = "";
			for (int j = 0; j < cells[0].length; j++) {
				line += cells[i][j]+",";
			}
			line = line.substring(0, line.length()-1);
			fw.write(line+System.getProperty("line.separator"));
		}
		fw.close();
	}
	public static String[][] appendColumnCSV(String[][] currentCells, String[] column) {
		String[][] newCells = new String[currentCells.length][currentCells[0].length+1];
		if (column.length != newCells.length) {
			throw new IllegalArgumentException("Column must be the same length as the current CSV");
		}
		for (int i = 0; i < newCells.length; i++) {
			for (int j = 0; j < newCells[0].length; j++) {
				if (j == currentCells[0].length) {
					newCells[i][j] = column[i];
				}
				else {
					newCells[i][j] = currentCells[i][j];
				}
			}
		}
		return newCells;
	}
	public static String[][] appendRowCSV(String[][] currentCells, String[] row) {
		String[][] newCells = new String[currentCells.length+1][currentCells[0].length];
		if (row.length != newCells[0].length) {
			throw new IllegalArgumentException("Column must be the same length as the current CSV");
		}
		for (int i = 0; i < newCells.length; i++) {
			for (int j = 0; j < newCells[0].length; j++) {
				if (i == currentCells.length) {
					newCells[i][j] = row[i];
				}
				else {
					newCells[i][j] = currentCells[i][j];
				}
			}
		}
		return newCells;
	}
}
