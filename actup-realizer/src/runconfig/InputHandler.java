package runconfig;

import grammar.Grammar;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Random;
import java.util.Set;

import org.jdom.Document;
import org.jdom.Element;

import realize.Realizer;
import synsem.LF;

public class InputHandler {
	private final Map<Integer, InputStruct[]> inputCache;
	private final TrainingSet ts;
	public InputHandler(Grammar g, String inputDirectory, TrainingSet ts, double percentToUse) {
		this.inputCache = prepareInputCache(g, inputDirectory, percentToUse);
		this.ts = ts;
	}
	public Set<Integer> getFiles() {
		return inputCache.keySet();
	}
	public Queue<InputStruct[]> getCurrentLockQueue(int currentLock) {
		int[] set = LMHandler.getFileSetFromLMNum(ts, currentLock);
		Queue<InputStruct[]> files = new LinkedList<InputStruct[]>();
		for (Integer i : set) {
			if (inputCache.containsKey(i)) {
				files.offer(inputCache.get(i));
			}
		}
		return files;
	}
	private static Map<Integer, InputStruct[]> prepareInputCache(Grammar g, String inputDirectory, double percentToUse) {
		Map<Integer, InputStruct[]> cachedInputs = new HashMap<Integer, InputStruct[]>();
		Queue<File> files = getInputFiles(inputDirectory, percentToUse);
		for (File f : files) {
			int fileNum = parseRunNum(f.getPath().toString());
			if (fileNum == -1) { 
				System.err.println(f.toPath() + ": is not a valid input file. It needs to have a number."); 
				System.exit(1); 
			}
			cachedInputs.put(fileNum, getInputStruct(g, f.getPath().toString()));
		}
		return cachedInputs;
	}
	private static Queue<File> getInputFiles(String dir, double percentToUse) {
		List<File> allInput = new ArrayList<File>(Arrays.asList(new File(dir).listFiles()));
		int numFilesToUse = Math.max(1, (int) Math.round((double)allInput.size() * percentToUse));
		List<File> out = new ArrayList<File>();
		Random r = new Random();
		for (int i = 0; i < numFilesToUse; i++) {
			//no repeats!
			int fileIndex = r.nextInt(allInput.size());
			out.add(allInput.get(fileIndex));
			allInput.remove(fileIndex);
		}
		Collections.sort(out);
		return new LinkedList<File>(out);
	}
	private static InputStruct[] getInputStruct(Grammar grammar, String inputfile) {			
		List<Element> items = prepareInput(grammar, inputfile);
		String[] goals = getGoals(items);
		InputStruct[] inp = new InputStruct[items.size()];
		for (int i = 0; i < items.size(); i++) {
			Element item = items.get(i);
			Element lfelt = item.getChild("lf");
			LF lf = Realizer.getLfFromElt(grammar.getLexicon(), lfelt, grammar.getGrammarsTypesData());
			inp[i] = new InputStruct(lf, goals[i], parseRunNum(inputfile));
		}
		return inp;
	}
	private static Document loadInput(Grammar grammar, String inputfile) throws IOException {
		return grammar.loadFromXml(inputfile);
	}

	private static List<Element> prepareInput(Grammar grammar, String inputfile) {
		Document doc = null;
		try {
			doc = loadInput(grammar, inputfile);
		}
		catch (IOException io) {
			io.printStackTrace();
			System.err.println("Error loading input file");
			System.exit(1);
		}
		Element root = doc.getRootElement();
		@SuppressWarnings("unchecked")
		List<Element> items = root.getChildren("item");
		List<Element> toRemove = new ArrayList<Element>();
		for (Element item : items) {
			String parseAttr = item.getAttribute("numOfParses").getValue();
			if (parseAttr.equals("0")) {
				toRemove.add(item);
			}
		}
		for (Element remove : toRemove) {
			items.remove(remove);
		}
		return items;
	}
	//extracts the targets from the xml sheet
	private static String[] getGoals(List<Element> items) {
		String[] goals = new String[items.size()];
		for (int i = 0; i < items.size(); i++) {
			goals[i] = items.get(i).getAttribute("string").getValue();
		}
		return goals;
	}
	public static Integer parseRunNum(String name) {
		String[] filepathsec = name.split("\\/");
		String fname = filepathsec[filepathsec.length-1];
		String[] fnameandext = fname.split("\\.");
		String fnamenoext = fnameandext[0];
		String[] sections = fnamenoext.split("\\-");
		for (String section : sections) {
			try {
				return Integer.parseInt(section);
			}
			catch (NumberFormatException nfe) {
				continue;
			}
		}
		return -1;
	}	
}
