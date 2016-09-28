package edu.psu.acs.lang.model;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import edu.psu.acs.lang.core.IModelElement;
import edu.psu.acs.lang.declarative.CCGCompoundType;
import edu.psu.acs.lang.declarative.CCGOperator;
import edu.psu.acs.lang.declarative.CCGOperatorEnum;
import edu.psu.acs.lang.declarative.CCGType;
import edu.psu.acs.lang.declarative.CCGTypeSlot;
import edu.psu.acs.lang.declarative.ChunkType;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ConjEnum;
import edu.psu.acs.lang.declarative.Conjable;
import edu.psu.acs.lang.declarative.EmptyChunk;
import edu.psu.acs.lang.declarative.EmptyEnum;
import edu.psu.acs.lang.declarative.LSSlotName;
import edu.psu.acs.lang.declarative.LSSlotNameEnum;
import edu.psu.acs.lang.declarative.LexSyn;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Sentence;
import edu.psu.acs.lang.declarative.SlotName;
import edu.psu.acs.lang.declarative.Word;
import edu.psu.acs.lang.production.AddLexSyn;
import edu.psu.acs.lang.production.BackwardApplication;
import edu.psu.acs.lang.production.BackwardComposition;
import edu.psu.acs.lang.production.ClearRetrievalError;
import edu.psu.acs.lang.production.ConjunctionLeft;
import edu.psu.acs.lang.production.ConjunctionRight;
import edu.psu.acs.lang.production.FinishConjunctionLeft;
import edu.psu.acs.lang.production.FinishConjunctionRight;
import edu.psu.acs.lang.production.FlushRetrievalLS;
import edu.psu.acs.lang.production.FlushRetrievalType;
import edu.psu.acs.lang.production.FocusHome;
import edu.psu.acs.lang.production.ForwardApplication;
import edu.psu.acs.lang.production.ForwardComposition;
import edu.psu.acs.lang.production.GrabWord;
import edu.psu.acs.lang.production.ResolveBackwardApplication;
import edu.psu.acs.lang.production.ResolveBackwardComposition;
import edu.psu.acs.lang.production.ResolveForwardApplication;
import edu.psu.acs.lang.production.ResolveForwardComposition;
import edu.psu.acs.lang.production.RetrieveHome;
import edu.psu.acs.lang.production.SyntaxRule;

public class ModelCreator {
	private final int largestSentenceK; //93 is overall on swbd; //i
	private final int mostTypesN; //136 is overall on swbd; //j
	
//	private final Path typesPath;
//	private final Path wordsPath;
//	private final Path sentPath;
	//private final File[] sentFiles;
	public ModelCreator(int maxwords, int mostTypes) {
//		prep.createWords(delimiter);
//		largestSentenceK = prep.maxWordsPerSentence();
//		mostTypesN = prep.createTypes();
//		this.typesPath = typesPath;
//		this.wordsPath = wordsPath;
//		this.sentPath = sentPath;
		this.largestSentenceK = maxwords;
		this.mostTypesN = mostTypes;
	}
	public List<IModelElement> makeChunkTypes() {
		List<IModelElement> toret = new ArrayList<IModelElement>();
		toret.add(new ChunkType(ChunkTypeEnum.CCGType, new ArrayList<SlotName>(Arrays.asList(CCGTypeSlot.values()))));
		
		//lexssyns
		List<SlotName> lsslots = new ArrayList<SlotName>();
		lsslots.add(new LSSlotName(LSSlotNameEnum.Word));
		for (int j = 1; j <= mostTypesN; j++) {
			lsslots.add(new LSSlotName(LSSlotNameEnum.Combinator, j, mostTypesN));
			lsslots.add(new LSSlotName(LSSlotNameEnum.LeftType, j, mostTypesN));
			lsslots.add(new LSSlotName(LSSlotNameEnum.RightType, j, mostTypesN));
			lsslots.add(new LSSlotName(LSSlotNameEnum.Type, j, mostTypesN));
			lsslots.add(new LSSlotName(LSSlotNameEnum.Conj, j, mostTypesN));
		}
		toret.add(new ChunkType(ChunkTypeEnum.lexsyn, lsslots));
		toret.add(new ChunkType(ChunkTypeEnum.word, new ArrayList<SlotName>()));
		toret.add(new ChunkType(ChunkTypeEnum.operator, new ArrayList<SlotName>()));
		toret.add(new ChunkType(ChunkTypeEnum.conj, new ArrayList<SlotName>()));
		toret.add(new ChunkType(ChunkTypeEnum.empty, new ArrayList<SlotName>()));
		
		//sentences 
		List<SlotName> sslots = new ArrayList<SlotName>();
		for (int i = 1; i <= largestSentenceK; i++) {
			sslots.add(new SSlotName(SSlotNameEnum.WordSem, i, Sentence.getWorkingMemorySize()));
			sslots.add(new SSlotName(SSlotNameEnum.LexsynString, i, Sentence.getWorkingMemorySize()));
			for (int j = 1; j <= mostTypesN; j++) {
				sslots.add(new SSlotName(SSlotNameEnum.LexsynCombo, i, Sentence.getWorkingMemorySize(), j, mostTypesN));
				sslots.add(new SSlotName(SSlotNameEnum.LexsynLeftType, i, Sentence.getWorkingMemorySize(), j, mostTypesN));
				sslots.add(new SSlotName(SSlotNameEnum.LexsynRightType, i, Sentence.getWorkingMemorySize(), j, mostTypesN));
				sslots.add(new SSlotName(SSlotNameEnum.LexsynFullType, i, Sentence.getWorkingMemorySize(), j, mostTypesN));
				sslots.add(new SSlotName(SSlotNameEnum.LexsynConj, i, Sentence.getWorkingMemorySize(), j, mostTypesN));
			}
		}
		toret.add(new ChunkType(ChunkTypeEnum.sentence, sslots));
		
		//sentence manager
	//	List<SlotName> smSlots = new ArrayList<SlotName>();
	//	smSlots.add(new SingletonSlotName(SingletonSlotNameEnum.goal));
	//	toret.add(new ChunkType(ChunkTypeEnum.sentenceManager, smSlots));
		
		
		return toret;
	}
	public List<IModelElement> makeEmptyChunk() {
		List<IModelElement> ele = new ArrayList<IModelElement>();
		ele.add(new EmptyChunk(EmptyEnum.NV));
		ele.add(new EmptyChunk(EmptyEnum.NA));
		return ele;
	}
	public List<IModelElement> makeOperatorChunks() {
		List<IModelElement> ele = new ArrayList<IModelElement>();
		ele.add(new CCGOperator(CCGOperatorEnum.Slash));
		ele.add(new CCGOperator(CCGOperatorEnum.Backslash));
		return ele;
	}
	public List<IModelElement> makeConjChunks() {
		List<IModelElement> el = new ArrayList<IModelElement>();
		el.add(new Conjable(ConjEnum.Conjable));
		el.add(new Conjable(ConjEnum.Nonconjable));
		return el;
	}
	public List<IModelElement> makeTypeChunks(Path typesList) throws IOException {
		List<IModelElement> ele = new ArrayList<IModelElement>();
		Set<CCGType> types = new HashSet<CCGType>();
		List<String> typelines = Files.readAllLines(typesList);
		for (String line : typelines) {
			CCGType cg = CCGCompoundType.makeCCGType(line, true);
			if (cg instanceof CCGCompoundType) {
				CCGCompoundType ccgt = (CCGCompoundType) cg;
				types.addAll(ccgt.harvestTypes());
			}
			else {
				types.add(cg);
			}
		}
		boolean haveTypestoAdd = true;
		Set<CCGType> newTypes = new HashSet<CCGType>();
		while (haveTypestoAdd) {
			haveTypestoAdd = false;
			for (CCGType cg1 : types) {	
				for (CCGType cg2 : types) {
					//will try both as the left and the right due to the embedded for loops
					CCGType bapp = SyntaxRule.BApp(cg1, cg2);
					if (bapp != null && !types.contains(bapp)) {
						newTypes.add(bapp);
						haveTypestoAdd = true;
					}
					CCGType fapp = SyntaxRule.FApp(cg1, cg2);
					if (fapp != null && !types.contains(fapp)) {
						newTypes.add(fapp);
						haveTypestoAdd = true;
					}
					CCGType fcomp = SyntaxRule.FComp(cg1, cg2);
					if (fcomp != null && !types.contains(fcomp)) {
						newTypes.add(fcomp);
						haveTypestoAdd = true;
					}
					CCGType bcomp = SyntaxRule.BComp(cg1, cg2);
					if (bcomp != null && !types.contains(bcomp)) {
						newTypes.add(bcomp);
						haveTypestoAdd = true;
					}
				}
			}
			for (CCGType cg : newTypes) {
				types.add(cg);
				if (cg instanceof CCGCompoundType) {
					CCGCompoundType ccgt = (CCGCompoundType) cg;
					types.addAll(ccgt.harvestTypes());
				}
			}
			newTypes.clear();
		}
		ele.addAll(types);
		return ele;
	}
	public List<IModelElement> makeLexSynAndWordChunks(Path wordInfo, String delimiter) throws IOException {
		List<IModelElement> ele = new ArrayList<IModelElement>();
		List<LexSyn> lexsyns = new ArrayList<LexSyn>();
		Set<Word> words = new HashSet<Word>();
		List<String> wordlines = Files.readAllLines(wordInfo);
		for (String line : wordlines) {
			String[] l = line.split(delimiter);
			if (l.length < 2) {
				System.err.println("Line does not contain bare minimum of type and word");
				System.err.println(l);
				System.exit(1);
			}
			//System.out.println("Word:"+l[0]);
			List<String> typerz = new ArrayList<String>(Arrays.asList(l));
			words.add(new Word(typerz.remove(0))); //removing the word itself
			List<CCGType> typerzTypes = new ArrayList<CCGType>();
			for (String f : typerz) {
				typerzTypes.add(CCGCompoundType.makeCCGType(f, true));
			}
			lexsyns.add(LexSyn.makeLexSyn(l[0], typerzTypes, mostTypesN));
		}
		ele.addAll(words);
		ele.addAll(lexsyns);
		return ele;
	}
	public List<Sentence> makeSentences(Path sentPath) throws IOException {
		List<Sentence> ele = new ArrayList<Sentence>();
		List<Sentence> goals = new ArrayList<Sentence>();
		List<String> sentlines = Files.readAllLines(sentPath);
		int k = 1;
		for (String line : sentlines) {
			List<String> wordBag = new ArrayList<String>(Arrays.asList(line.split(" ")));
			goals.add(Sentence.makeSentence(k, wordBag, mostTypesN, largestSentenceK));
			k++;
		}
		ele.addAll(goals);
	
		return ele;
	}
//	public List<IModelElement> makeSentenceManagers(List<Sentence> allSentences) {
//		List<IModelElement> ele = new ArrayList<IModelElement>();
//		for (int i = 1; i <= allSentences.size(); i++) {
//			ele.add(SentenceManager.makeSentenceManager(i, allSentences.get(i-1)));
//		}
//		return ele;
//	}
//	
	public List<IModelElement> makeRules() {
		List<IModelElement> rules = new ArrayList<IModelElement>();
		
		rules.add(new FlushRetrievalLS());
		rules.add(new FlushRetrievalType());
		rules.add(new FocusHome());
	//	rules.add(new FocusNewGoal());
		rules.add(new RetrieveHome());
		rules.add(new ClearRetrievalError());
//		for (int j = 1; j <= numSentences; j++) {
//			rules.add(new RetrieveHome(j));
//		}
		for (int j = 1; j <= largestSentenceK; j++) {		
			rules.add(new GrabWord(j, largestSentenceK));
		}
		for (int j = 1; j <= Sentence.getWorkingMemorySize(); j++) {
			rules.add(new AddLexSyn(j, mostTypesN, Sentence.getWorkingMemorySize()));
			for (int i = 1; i <= mostTypesN; i++) {
				rules.add(new ResolveForwardApplication(j, i, Sentence.getWorkingMemorySize(), mostTypesN));
				rules.add(new ResolveBackwardApplication(j, i, Sentence.getWorkingMemorySize(), mostTypesN));
				rules.add(new ResolveForwardComposition(j, i, Sentence.getWorkingMemorySize(), mostTypesN));
				rules.add(new ResolveBackwardComposition(j, i, Sentence.getWorkingMemorySize(), mostTypesN));
				for (int k = 1; k <= Sentence.getWorkingMemorySize(); k++) {
					if (k == j) {
						continue; //words can't join with themselves, still!
					}
					for (int l = 1; l <= mostTypesN; l++) {	
						rules.add(new ForwardApplication(j, i, k, l, mostTypesN, Sentence.getWorkingMemorySize()));
						rules.add(new BackwardApplication(j, i, k, l, mostTypesN, Sentence.getWorkingMemorySize()));
						rules.add(new ForwardComposition(j, i, k, l, mostTypesN, Sentence.getWorkingMemorySize()));
						rules.add(new BackwardComposition(j, i, k, l, mostTypesN, Sentence.getWorkingMemorySize()));
						rules.add(new ConjunctionLeft(j, i, k, l, mostTypesN, Sentence.getWorkingMemorySize()));
						rules.add(new ConjunctionRight(j, i, k, l, mostTypesN, Sentence.getWorkingMemorySize()));
						rules.add(new FinishConjunctionLeft(j, i, k, l, mostTypesN, Sentence.getWorkingMemorySize()));
						rules.add(new FinishConjunctionRight(j, i, k, l, mostTypesN, Sentence.getWorkingMemorySize()));
					}
				}
			}
		}
		
		return rules;
	}
	
}
