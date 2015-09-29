package edu.psu.acs.lang;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import edu.psu.acs.lang.declarative.CCGCompoundType;
import edu.psu.acs.lang.declarative.CCGType;
import edu.psu.acs.lang.declarative.CCGTypeSlot;
import edu.psu.acs.lang.declarative.ChunkType;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.LSSlotName;
import edu.psu.acs.lang.declarative.LSSlotNameEnum;
import edu.psu.acs.lang.declarative.LexSyn;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Sentence;
import edu.psu.acs.lang.declarative.SlotName;
import edu.psu.acs.lang.production.AddLexSyn;
import edu.psu.acs.lang.production.BackwardApplication;
import edu.psu.acs.lang.production.BackwardComposition;
import edu.psu.acs.lang.production.ChooseFirstWord;
import edu.psu.acs.lang.production.ForwardApplication;
import edu.psu.acs.lang.production.ForwardComposition;
import edu.psu.acs.lang.production.GrabWord;
import edu.psu.acs.lang.production.ResolveBackwardApplication;
import edu.psu.acs.lang.production.ResolveBackwardComposition;
import edu.psu.acs.lang.production.ResolveForwardApplication;
import edu.psu.acs.lang.production.ResolveForwardComposition;

public class ModelCreator {
	public static final int largestSentenceK = 93; //i
	public static final int mostTypesN = 136; //j
	List<IModelElement> elements;
	FileWriter fw;
	private ModelCreator(String fname) throws IOException {
		elements = new ArrayList<IModelElement>();
		elements.add(new SimpleElement("<declarative-memory>"));
		elements.addAll(makeChunkTypes());
		elements.addAll(makeChunks());
		elements.add(new SimpleElement("</declarative-memory>"));
		elements.add(new SimpleElement("<procedural-memory>"));
		elements.addAll(makeRules());
		elements.add(new SimpleElement("</procedural-memory>")); 
		fw = new FileWriter(fname);
	}
	private void writeAll() throws IOException {
		for (IModelElement ime : elements) {
			for (String s : ime.toXML()) {
				fw.write(s+System.getProperty("line.separator"));
			}
		}
		fw.flush();
	}
	//first, model can read all the types (from types.txt) and load them into a CCGType.
		//importantly, every type has a variety of subtypes. These, down to the base type, need to be stored.
		//For model simplicity's sake, they will be stored as part of one chunk...
	//next, model can load lexsyns from words.dsv
	//next model will loop to create the Sentence with all of its beautiful slots
	//lastly the model will loop to create all of the rules
	private static List<IModelElement> makeChunkTypes() {
		List<IModelElement> toret = new ArrayList<IModelElement>();
		toret.add(new ChunkType(ChunkTypeEnum.CCGType, new ArrayList<SlotName>(Arrays.asList(CCGTypeSlot.values()))));
		
		List<SlotName> lsslots = new ArrayList<SlotName>();
		lsslots.add(new LSSlotName(LSSlotNameEnum.Word));
		for (int j = 1; j <= mostTypesN; j++) {
			lsslots.add(new LSSlotName(LSSlotNameEnum.Combinator, j));
			lsslots.add(new LSSlotName(LSSlotNameEnum.LeftType, j));
			lsslots.add(new LSSlotName(LSSlotNameEnum.RightType, j));
			lsslots.add(new LSSlotName(LSSlotNameEnum.Type, j));
		}
		toret.add(new ChunkType(ChunkTypeEnum.lexsyn, lsslots));
		
		List<SlotName> sslots = new ArrayList<SlotName>();
		sslots.add(new SSlotName(SSlotNameEnum.FullType));
		sslots.add(new SSlotName(SSlotNameEnum.RightFullType));
		sslots.add(new SSlotName(SSlotNameEnum.LeftFullType));
		sslots.add(new SSlotName(SSlotNameEnum.Combinator));
		for (int i = 1; i <= largestSentenceK; i++) {
			sslots.add(new SSlotName(SSlotNameEnum.Word, i));
			sslots.add(new SSlotName(SSlotNameEnum.Cue, i));
			for (int j = 1; j <= mostTypesN; j++) {
				sslots.add(new SSlotName(SSlotNameEnum.CueCombo, i, j));
				sslots.add(new SSlotName(SSlotNameEnum.CueLeftType, i, j));
				sslots.add(new SSlotName(SSlotNameEnum.CueRightType, i, j));
				sslots.add(new SSlotName(SSlotNameEnum.CueType, i, j));
			}
		}
		
		return toret;
	}
	private static List<IModelElement> makeChunks() throws IOException {
		//sentence will obviously start off empty... (this could change as a reasonable method of learning?)
		//CCGTypes, read from types.txt. Importantly, we also want all subtypes of all of these types to be added. In a set-like way.
		List<IModelElement> ele = new ArrayList<IModelElement>();
		Set<CCGType> types = new HashSet<CCGType>();
		List<String> typelines = Files.readAllLines(Paths.get("/work/research/actup-production/data/types.txt"));
		int h = 0;
		for (String line : typelines) {
			CCGType cg = CCGCompoundType.makeCCGType(line);
			if (cg instanceof CCGCompoundType) {
				CCGCompoundType ccgt = (CCGCompoundType) cg;
				types.addAll(ccgt.harvestTypes());
			}
			else {
				types.add(cg);
			}
			//System.out.println(h);
			h++;
		}
		ele.addAll(types);
		
		List<LexSyn> lexsyns = new ArrayList<LexSyn>();
		List<String> wordlines = Files.readAllLines(Paths.get("/work/research/actup-production/data/words.dsv"));
		for (String line : wordlines) {
			String[] l = line.split(":-:");
			if (l.length < 2) {
				System.err.println("Line does not contain bare minimum of type and word");
				System.err.println(l);
				System.exit(1);
			}
			//System.out.println("Word:"+l[0]);
			List<String> typerz = new ArrayList<String>(Arrays.asList(l));
			typerz.remove(0); //removing the word itself
			List<CCGType> typerzTypes = new ArrayList<CCGType>();
			for (String f : typerz) {
				typerzTypes.add(CCGCompoundType.makeCCGType(f));
			}
			lexsyns.add(LexSyn.makeLexSyn(l[0], typerzTypes, mostTypesN));
		}
		ele.addAll(lexsyns);
		
		List<Sentence> goals = new ArrayList<Sentence>();
		List<String> sentlines = Files.readAllLines(Paths.get("/work/research/actup-production/data/swbd.txt"));
		int k = 1;
		for (String line : sentlines) {
			List<String> wordBag = new ArrayList<String>(Arrays.asList(line.split(" ")));
			goals.add(Sentence.makeSentence("goal"+k, wordBag, mostTypesN, largestSentenceK));
			k++;
		}
		ele.addAll(goals);
//		//lexsyns, read from words.dsv
		return ele;
	}
	private static List<IModelElement> makeRules() {
		List<IModelElement> rules = new ArrayList<IModelElement>();
		for (int j = 1; j <= largestSentenceK; j++) {
			rules.add(new AddLexSyn(j, mostTypesN));
			rules.add(new GrabWord(j));
			rules.add(new ChooseFirstWord(j, mostTypesN));
			for (int i = 1; i <= mostTypesN; i++) {
				rules.add(new ForwardApplication(j, i, mostTypesN));
				rules.add(new BackwardApplication(j, i, mostTypesN));
				rules.add(new ForwardComposition(j, i, mostTypesN));
				rules.add(new BackwardComposition(j, i, mostTypesN));
			}
		}
		rules.add(new ResolveForwardApplication());
		rules.add(new ResolveBackwardApplication());
		rules.add(new ResolveForwardComposition());
		rules.add(new ResolveBackwardComposition());
		return rules;
	}
	
	public static void main(String[] args) throws IOException {
		//createWords();
		//createTypes();
		ModelCreator tc = new ModelCreator("/work/research/actup-production/data/test-model.jactr");
		tc.writeAll();
	}
	private static void createTypes() throws IOException {
		List<String> lines = Files.readAllLines(Paths.get("/work/research/actup-production/data/words.dsv"));
		FileWriter write = new FileWriter("/work/research/actup-production/data/types.txt");
		Set<String> types = new HashSet<String>();
		for (String line : lines) {
			String[] separated = line.split(":-:");
			for (int i = 1; i < separated.length; i++) {
				types.add(separated[i]);
			}
		}
		for (String type : types) {
			write.write(type+"\n");
		}
		write.close();
		
	}
	private static void createWords() throws IOException {
		//first, we'll just grab the word and make a list of all of their types so that we have a clear idea
		List<String> lines = Files.readAllLines(Paths.get("/work/research/actup-production/data/swbd1.ccg"));
		lines.addAll(Files.readAllLines(Paths.get("/work/research/actup-production/data/swbd2.ccg")));
		lines.addAll(Files.readAllLines(Paths.get("/work/research/actup-production/data/swbd3.ccg")));
		lines.addAll(Files.readAllLines(Paths.get("/work/research/actup-production/data/swbd4.ccg")));
		lines.addAll(Files.readAllLines(Paths.get("/work/research/actup-production/data/swbd5.ccg")));
		FileWriter write = new FileWriter("/work/research/actup-production/data/words.dsv");
		HashMap<String, Set<String>> map = new HashMap<String, Set<String>>();
		for (String line : lines) {
			int start = line.indexOf('{');
			int end = line.indexOf('}');
			if (start == -1 || end == -1) {
				continue;
			}
			String fragment = line.substring(start, end);
			String[] wordParts = fragment.split(" ");
			
			//we have some kind of embedded bullshit. Most likely just have to move the starter pointer up
			while (wordParts.length > 4) {
				fragment = fragment.substring(1);
				int newStart = fragment.indexOf('{');
				fragment = fragment.substring(newStart);
				wordParts = fragment.split(" ");
			}
			List<String> parts = new ArrayList<String>(Arrays.asList(wordParts));
			parts.remove("");
			if (parts.contains(" ")) {
				parts.remove(" ");
				
			}
			
			//ok, now we most likely either have a type raise operation or we have a just a word with its type.
			
			//a "just in case" measure, if I forgot something
			if (parts.size() == 1) {
				System.err.println("No spaces in fragment, printing and exiting:");
				System.err.println(wordParts[0]);
				System.exit(1);
			}
			else if (parts.size() == 3 || (parts.size() == 4 && !parts.get(1).contains("T"))) {
				parts = new ArrayList<String>(Arrays.asList(new String[] { parts.get(1), parts.get(2) }));
			}
			
			String secondType = "";
			String type = "";
			String word = "";
			try {
				secondType = parts.size() == 4 ? parts.get(2).substring(1) : "";
				type = parts.get(0).substring(1);
				int wordIndex = parts.size() == 4 ? 3 : 1;
				word = parts.get(wordIndex);
			}
			catch (Exception e) {
				e.printStackTrace();
				System.err.println(parts.size());
				for (String part : parts) {
					System.err.println(part);
				}
				System.err.println(fragment);
				System.exit(1);
			}
			word = word.toLowerCase();
			
			//create or update the typeset
			Set<String> typeset;
			if (map.containsKey(word)) {
				typeset = map.get(word);
			}
			else {
				typeset = new HashSet<String>();
				map.put(word, typeset);
			}
			typeset.add(type);
			if (secondType != "") {
				typeset.add(secondType);
			}
		}
		for (Entry<String, Set<String>> pair : map.entrySet()) {
			if (pair.getValue().contains("CODE")) {
				continue;
			}
			String toWrite = pair.getKey()+":-:";
			for (String type : pair.getValue()) {
				toWrite += type+":-:";
			}
			toWrite = toWrite.substring(0, toWrite.length()-3) + "\n";
			write.write(toWrite);
		}
		write.close();
	}
}
