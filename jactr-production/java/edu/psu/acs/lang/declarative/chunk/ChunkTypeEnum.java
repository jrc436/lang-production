package edu.psu.acs.lang.declarative.chunk;

import java.util.List;

import edu.psu.acs.lang.core.IModelElement;
import edu.psu.acs.lang.declarative.Word;
import edu.psu.acs.lang.declarative.lexsyn.LexSyn;
import edu.psu.acs.lang.declarative.sentence.Sentence;
import edu.psu.acs.lang.declarative.type.CCGOperator;
import edu.psu.acs.lang.declarative.type.Conjable;

public enum ChunkTypeEnum implements IModelElement {
	lexsyn,
	sentence,
	word,
	operator,
	empty,
	conj,
	//state,
	//sentenceManager,
	CCGType;
	private static int maxtypes = -1;
	private static int maxwords = -1;
	public static void initializeAsElements(int maxt, int maxw) {
		maxtypes = maxt;
		maxwords = maxw;
	}
	public List<String> toXML() {
		if (maxtypes == -1 || maxwords == -1) {
			throw new UnsupportedOperationException("Please first call initializeAsElements");
		}
		switch (this) {
			case CCGType:
				return edu.psu.acs.lang.declarative.type.CCGType.factory().chunkTypeXML();
			case conj:
				return Conjable.factory().chunkTypeXML();
			case empty:
				return EmptyChunk.factory().chunkTypeXML();
			case lexsyn:
				return LexSyn.factory(maxtypes).chunkTypeXML();
			case operator:
				return CCGOperator.factory().chunkTypeXML();
			case sentence:
				return Sentence.factory(maxtypes, maxwords).chunkTypeXML();
			case word:
				return Word.factory().chunkTypeXML();
			default:
				throw new UnsupportedOperationException("Ensure the switch statement has been successfully added to after creating your ChunkTypeEnum");
			}
	}
}
