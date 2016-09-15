package edu.psu.acs.lang.eval.target;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import edu.psu.acs.lang.RunConsts;
import edu.psu.acs.lang.eval.ngram.GramSet;
import edu.psu.acs.lang.output.OutputReader;
import edu.psu.acs.lang.output.OutputSentence;

public class Corrector {
	private static List<GramSet> getInputMap() throws IOException {
		File[] fileList = RunConsts.getInputFilesList();
		List<GramSet> fidMap = new ArrayList<GramSet>();
		for (File f : fileList) {
			fidMap.add(new GramSet(Files.readAllLines(f.toPath())));			
		}
		return fidMap;
	}
	private static List<GramSet> getOutputMap() {
		List<GramSet> idmap = new ArrayList<GramSet>();
		for (int i = 0; i < 50; i++) {
			List<OutputSentence> os = OutputReader.getAllSentences(RunConsts.getAllOutputFiles(i));
			List<String> sent = OutputReader.treeSentences(os);
			GramSet ws = new GramSet();
			for (String s : sent) {
				ws.addSentences(OutputReader.getFragments(s));
			}
			if (ws.isEmpty()) {
				idmap.add(null);
			}
			else {
				idmap.add(ws);
			}
		}
		return idmap;
	}
	private static Map<Integer, Integer> getCorrectMapping() throws IOException {
		List<GramSet> fidMap = getInputMap();
		List<GramSet> idmap = getOutputMap();
		
		Map<Integer, Integer> correctMapping = new HashMap<Integer, Integer>();
		//System.err.println(fidMap.get(34).setDiff(idmap.get(45)));
		for (int i = 0; i < idmap.size(); i++) {			
			boolean found = false;
			if (idmap.get(i) == null) {
				continue;
			}
		
			for (int j = 0; j < fidMap.size(); j++) {			
				if (fidMap.get(j).isSuperSet(idmap.get(i))) {
					if (!found) {
						found = true;
						correctMapping.put(j, i);
					}
					else {
						System.err.println("Multiple matches found. This is a critical error.");
						System.exit(1);
					}
				}
			}
			if (!found) {
				System.err.println("Set: "+i+" found no matches. Scanning for close matches");
				for (int j = 0; j < fidMap.size(); j++) {
					GramSet ws = fidMap.get(j).setDiff(idmap.get(i));
					if (ws.size() < 3) {
						found = true;
						correctMapping.put(j, i);
						continue;
					}
				}
			}
			if (!found) {
				System.err.println("Set:" +i+" will be excluded.");
			}
		}
		return correctMapping;
	}
	public static void main(String[] args) throws IOException {
		Map<Integer, Integer> correctMapping = getCorrectMapping();
		File[] fileList = RunConsts.getInputFilesList();
		for (int i = 0; i < fileList.length; i++) {
			if (!correctMapping.containsKey(i)) {
				System.err.println("Correct Mapping is missing key for: "+i);
				continue;
			}
			int newNum = correctMapping.get(i);
			FileWriter fw = new FileWriter(RunConsts.getFixedInputFile(newNum));
			for (String line : Files.readAllLines(fileList[i].toPath())) {
				fw.write(line + System.getProperty("line.separator"));
			}
			fw.close();
		}
		
	}
}
