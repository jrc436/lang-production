package util.listdata;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * This class does not extend the typical FileProcessor interface because it is NOT threadsafe. It relies on the files to be processed in order. 
 * However, it could be thought of a FileProcessor from DataCollection<E> to DataCollection<E>
 * @author jrc436
 *
 */
public class ReorderListProcessor {
	
	public enum Operation {
		FixNamesOnly,
		DataCollectionHeaders,
		PruneCSV;
		public static Operation fromString(String s) {
			for (Operation o : Operation.values()) {
				if (o.toString().equalsIgnoreCase(s)) {
					return o;
				}
			}
			return null;
		}
		public static void runOperation(Operation op, File inputDirectory, File outputDirectory, String rebase) {
			switch (op) {
				case DataCollectionHeaders:
					fixDataCollection(inputDirectory, outputDirectory, rebase);
					break;
				case FixNamesOnly:
					fixFileNames(inputDirectory, outputDirectory, rebase);
					break;
				case PruneCSV:
					pruneCSV(inputDirectory, outputDirectory, rebase);
					break;
				default:
					System.err.println("Operation not supported. Please check the name is correct. Valid operations include:");
					System.err.println(Operation.values());
					break;
			}
		}
		private static void pruneCSV(File inputDirectory, File outputDirectory, String rebase) {
			List<FileParts> fp = rebase(inputDirectory, outputDirectory, rebase);
			int[] colsData = prune(fp); 
			renumber(fp, outputDirectory);
			for (FileParts f : fp) {
				f.readFile();
				for (int i = 1; i < f.lines.size(); i++) {
					String newLine = "";
					String[] lineParts = f.lines.get(i).split(",");
					for (int j = 0; j < lineParts.length; j++) {
						if (colsData[j] > 0) {
							newLine += lineParts[j]+",";
						}
					}
					f.lines.set(i, newLine.substring(0, newLine.length()-1));
				}
				f.writeFile();
				f.clean();
			}
		}
		private static int[] prune(List<FileParts> fp) {
			fp.get(0).readFile();
			int columns = fp.get(0).lines.get(0).split(",").length;
			List<FileParts> prune = new ArrayList<FileParts>();
			int[] colHasData = new int[columns];
			for (FileParts f : fp) {
				f.readFile();
				for (int j = 1; j < f.lines.size(); j++) {
					String line = f.lines.get(j);
					String[] parts = line.split(",");
					boolean rowHasData = false;
					//first line will always be the header...
					for (int i = 0; i < parts.length; i++) {
						if (!parts[i].equals("null") && !parts[i].equals("0")) {
							colHasData[i]++;
							rowHasData = true;
						}
					}
					if (!rowHasData) {
						prune.add(f);
					}
				}
				f.clean();
			}
			for (FileParts f : prune) {
				fp.remove(f);
			}
			return colHasData;
		}
		private static void renumber(List<FileParts> fp,  File outputDirectory) {
			for (int i = 0; i < fp.size(); i++) {
				fp.get(i).fileNum = i;
			}
			addRepaths(outputDirectory, fp, null);
		}
		private static List<FileParts> rebase(File inputDirectory, File outputDirectory, String rebase) {
			List<FileParts> fp = captureAll(inputDirectory);
			Collections.sort(fp, FileParts.getFileNumComparator());
			fp.get(0).readFile();
			System.err.println(fp.get(0).toString());
			outputDirectory.mkdirs();
			addRepaths(outputDirectory, fp, rebase);
			return fp;
		}
		public static void fixDataCollection(File inputDirectory, File outputDirectory, String rebase) {
			List<FileParts> fp = rebase(inputDirectory, outputDirectory, rebase);
			String lastKeyLine = "";
			for (String s : fp.get(0).lines) { 
				if (DataCollection.isKeyLine(s)) {
					lastKeyLine = s;
				}
			}
			String lastNewKeyLine = lastKeyLine;
			String fileResponsible = fp.get(0).toString();
			for (FileParts f : fp) {
				System.err.println("Fixing:" + f.toString()+", using: "+fileResponsible);
				f.readFile();
				lastKeyLine = altFix(f.lines, lastKeyLine);
				if (!lastNewKeyLine.equals(lastKeyLine)) {
					fileResponsible = f.toString();
				}
				lastNewKeyLine = lastKeyLine;
				f.writeFile();
				f.clean();
			}
		}
		public static void fixFileNames(File inputDirectory, File outputDirectory, String rebase) {
			List<FileParts> fp = rebase(inputDirectory, outputDirectory, rebase);
			for (FileParts f : fp) {
				f.readFile();
				f.writeFile();
				f.clean();
			}
		}
	}
	
	
	public static void main(String[] args) throws IOException {
		if (args.length < 2) {
			System.err.println("Specify inputDirectory first, and whether the files also need their headers fixed / is a broken DataCollection (true or false)");
			System.err.println("Optionally, the third argument would be a non-default output directory, and the fourth would be a new basename");
			System.exit(1);
		}
		File inputDirectory = new File(args[0]);
		Operation op = Operation.fromString(args[1]);
		File outputDirectory = args.length > 2 ?  new File(args[2]) : inputDirectory.getParentFile().toPath().resolve("f"+inputDirectory.getName()).toFile();
		String rebase = args.length > 2 ? args[2] : null;
		Operation.runOperation(op, inputDirectory, outputDirectory, rebase);
	}
	private static String altFix(List<String> second, String lastKeyLine) {
		if (!DataCollection.isKeyLine(lastKeyLine)) {
			throw new IllegalArgumentException("The 'first' list has still not been fixed, so this call is invalid");
		}
		String lastKeyLine2 = lastKeyLine;
		for (String s : second) {
			if (DataCollection.isKeyLine(s)) {
				lastKeyLine2 = s;
			}
		}
		if (!DataCollection.isKeyLine(second.get(0))) {
			second.add(0, lastKeyLine);
		}
		return lastKeyLine2;
	}
	public static List<FileParts> captureAll(File inputDirectory) {
		List<FileParts> fileparts = new ArrayList<FileParts>();
		for (File f : inputDirectory.listFiles()) {
			fileparts.add(capture(f));
		}
		return fileparts;
	}
	private static Path getNewFilePath(File outputDir, FileParts fp, String rebase, int totalFiles) {
		if (rebase != null) {
			return outputDir.toPath().resolve(fp.rebaseFormat(rebase, totalFiles));
		}
		return outputDir.toPath().resolve(fp.format(totalFiles));
	}
	public static void addRepaths(File outputDir, List<FileParts> fp, String rebase) {
		int totalFiles = fp.size();
		for (FileParts f : fp) {
			f.repath = getNewFilePath(outputDir, f, rebase, totalFiles);
		}
	}
	public static void copyAll(List<FileParts> fp) throws IOException {
		for (FileParts f : fp)  {
			Files.copy(f.origFile.toPath(), f.repath);
		}
	}
	
	/**
	 * The naming conventions of files require there to be a basename-##.ext .
	 */
	private static FileParts capture(File f) {
		String fileName = f.getName();
		String[] dashParts = fileName.split("-");
		String numAndExt = "";
		String baseName = "";
		if (dashParts.length >= 2) {
			numAndExt = dashParts[dashParts.length-1];
			for (int i = 0; i < dashParts.length-1; i++) {
				baseName += dashParts[i] + "-";
			}
			baseName = baseName.substring(0, baseName.length()-1);
		}
		else {
			System.err.println("Warning: no dashes found in file. Parsing could be incorrect (parsed from : "+fileName+")");
			if (!fileName.contains(".")) {
				return new FileParts(fileName, "", -1, f);
			}
			String[] parts = fileName.split("\\.");
			baseName = parts[0];
			for (int i = 1; i < parts.length; i++) {
				numAndExt += parts[i];
			}
		}
		String[] extParse = numAndExt.split("\\.");
		if (extParse.length > 2) {
			System.err.println("Warning: multiple periods in file extension. Parsing could be incorrect (parsed from: "+numAndExt+")");
		}
		else if (extParse.length < 2) {
			System.err.println("Warning: no file extension found. Parsing could be incorrect (parsed from: "+numAndExt+")");
			if (numAndExt.matches("\\d+")) {
				return new FileParts(baseName, "", Integer.parseInt(numAndExt), f);
			}
			System.err.println("Warning: no number found. Parsing could be incorrect (parsed from: "+numAndExt+")");
			return new FileParts(baseName, "", -1, f);
		}
		int num = extParse[0].matches("\\d+") ? Integer.parseInt(extParse[0]) : -1;
		int s = num == -1 ? 0 : 1;
		String ext = "";
		for (int i = s; i < extParse.length; i++) {
			ext += extParse[i] + ".";
		}
		ext = ext.substring(0, ext.length()-1);
		return new FileParts(baseName, ext, num, f);
	}
	static class FileParts {
		private final String baseName;
		private int fileNum;
		private final String extension;
		private final File origFile;
		private Path repath;
		private List<String> lines;
		public static Comparator<FileParts> getFileNumComparator() {
			return new Comparator<FileParts>() {
				@Override
				public int compare(FileParts o1, FileParts o2) {
					if (o1.fileNum == o2.fileNum) {
						return 0;
					}
					else if (o1.fileNum > o2.fileNum) {
						return 1;
					}
					return -1;
				}
			};
		}
		public FileParts(String baseName, String extension, int fileNum, File origFile) {
			this.baseName = baseName;
			this.extension = extension;
			this.fileNum = fileNum;
			this.origFile = origFile;
		}
		private String extString() {
			return extension == null || extension.isEmpty() ? "" : "."+extension;
		}
		public String toString() {
			if (fileNum == -1) {
				return baseName + extString();
			}
			return baseName + "-" + fileNum  + extString();
		}
		
		public String format(int totalNumberOfFiles) {
			if (fileNum == -1) {
				if (totalNumberOfFiles != 1) {
					System.err.println("Warning: naming scheme is assigning a non-numbered file, despite having multiple files");
				}
				return baseName + extString();
			}
			int digitsNeeded = 1 + ((int)Math.floor(Math.log10(totalNumberOfFiles)));
			String formatNumber = String.format("%0"+digitsNeeded+"d", fileNum);
			return baseName + "-" + formatNumber + extString();
		}
		public String rebaseFormat(String newBase, int totalNumberOfFiles) {
			return new FileParts(newBase, extension, fileNum, origFile).format(totalNumberOfFiles);
		}
		public void readFile() {
			try {
				this.lines = Files.readAllLines(origFile.toPath());
			}
			catch (IOException ie) {
				System.err.println("Unable to read file: "+origFile);
				ie.printStackTrace();
				System.exit(1);
			}
		}
		public void clean() {
			this.lines = null;
		}
		public void writeFile() {
			try {
				FileWriter fw = new FileWriter(repath.toFile());
				for (String s : lines) {
					fw.write(s + System.getProperty("line.separator"));
				}
				fw.close();
			} catch (IOException e) {
				System.err.println("Unable to write to file: "+repath);
				e.printStackTrace();
				System.exit(1);
			}
		}
	}
}
