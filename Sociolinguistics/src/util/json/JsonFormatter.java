package util.json;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * The Reddit's default format is a list of Jsons, but JsonLayer requires a slightly different format. This changes the
 * format to what's required by JsonLayer.
 * @author jrc
 *
 */
public class JsonFormatter {
	public static void main(String[] args) {
		String flag = "-i";
		String helpMsg = "Two arguments are required, first, the input directory, then either the \""+flag+"\" flag or the output directory. The "+flag+" flag means to process in place, it should be followed by the string to append to the file names. Both directories should exist. ";
		if (args.length < 2 || args.length > 3) {
			System.err.println(helpMsg);
			System.exit(1);
		}
		File inpDirectory = Paths.get(args[0]).toFile();
		if (!inpDirectory.isDirectory()) {
			System.err.println(helpMsg);
			System.exit(1);
		}
		if (args[1].equals(flag)) {
			if (args[2].equals("")) {
				System.err.println(helpMsg);
				System.exit(1);
			}
			String append = args[2];
			System.out.println("Using: " + append + " as the append string");
			processInPlace(Paths.get(args[0]), append);
		}
		else {
			File outDirectory = Paths.get(args[1]).toFile();
			if (!outDirectory.isDirectory()) {
				System.err.println(helpMsg);
				System.exit(1);
			}
			processAndSplit(inpDirectory.toPath(), outDirectory.toPath());
		} 
	}
	private static void processFile(File f, String mod) {
		try {
			List<String> lines = Files.readAllLines(f.toPath());
			if (lines.isEmpty()) {
				System.err.println(f.toPath()+ " is empty, can't process");
				return;
			}
			FileWriter fw = new FileWriter(f.toPath().toString()+mod);
			fw.write("["+System.getProperty("line.separator"));
			for (int i = 0; i < lines.size(); i++) {
				if (i == lines.size()-1) {
					fw.write(lines.get(i)+System.getProperty("line.separator"));
				}
				else {
					fw.write(lines.get(i)+","+System.getProperty("line.separator"));
				}
			}
			fw.write("]");
			fw.close();
		} catch (IOException e) {
			System.err.println("Error reading "+f.toPath());
			System.err.println(e.getMessage());
			return;
		}
	}	
	/**
	 * small files
	 */
	public static void processInPlace(Path inpDirectory, String mod) {
		File[] listOfFiles = inpDirectory.toFile().listFiles();
		for (File f : listOfFiles) {
			processFile(f, mod);
		}
	}
	public static String collectJsons(List<JsonReadable> transformation) {
		String ret = "["+System.getProperty("line.separator");
		for (int i = 0; i < transformation.size(); i++) {
			if (i == transformation.size() -1) {
				ret += transformation.get(i).toString()+System.getProperty("line.separator");
			}
			else {
				ret += transformation.get(i).toString()+","+System.getProperty("line.separator");
			}
		}
		ret += "]";
		return ret;
	}
	/**
	 * big files
	 * @param inp
	 * @param outFolder
	 */
	public static void processAndSplit(Path inpFolder, Path outFolder) {
		Scanner s;
		Path writePath = null;
		FileWriter fr1;
		File[] files = inpFolder.toFile().listFiles();
		try {		
			for (File inp : files) {
				int fileNum = 0;
				String numAppend = String.format("%03d", fileNum);
				s = new Scanner(inp);
				
				writePath = outFolder.resolve(inp.getName()+numAppend+".mod");
				
				List<String> lines = new ArrayList<String>();
				while (s.hasNextLine()) {
					String line = s.nextLine();
					if (line.trim().isEmpty()) {
						continue;
					}
					lines.add(line);
					if (lines.size() >= 10000) {
						fr1 = new FileWriter(writePath.toFile());
						fr1.write("["+System.getProperty("line.separator"));
						for (int i = 0; i < lines.size(); i++) {
							if (i == lines.size() -1) {
								fr1.write(lines.get(i)+System.getProperty("line.separator"));
							}
							else {
								fr1.write(lines.get(i)+","+System.getProperty("line.separator"));
							}
						}
						fr1.write("]");
						fr1.close();
						lines.clear();
						fileNum++;
						numAppend = String.format("%03d", fileNum);
						writePath = outFolder.resolve(inp.getName()+numAppend+".mod");
					}
				}
				//final write
				fr1 = new FileWriter(writePath.toFile());
				fr1.write("["+System.getProperty("line.separator"));
				for (int i = 0; i < lines.size(); i++) {
					if (i == lines.size() -1) {
						fr1.write(lines.get(i)+System.getProperty("line.separator"));
					}
					else {
						fr1.write(lines.get(i)+","+System.getProperty("line.separator"));
					}
				}
				fr1.write("]");
				fr1.close();
				s.close();
			} 
		}
		catch (FileNotFoundException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}
}
