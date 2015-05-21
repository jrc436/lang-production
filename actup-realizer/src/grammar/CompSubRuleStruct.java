package grammar;

import lexicon.LexicalData;
import lexicon.Lexicon;
import lexicon.Tokenizer;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;
import unify.UnifyControl;

public class CompSubRuleStruct extends RuleStruct {
	private final boolean isHarmonic;
	public CompSubRuleStruct(RuleType rt, Lexicon l, boolean isHarmonic, Tokenizer t) {
		super(rt, l, t);
		this.isHarmonic = isHarmonic;
	}
	public Rule getRuleFromStruct(UnifyControl uc, LexicalData lex) {
		switch (rt) {			
			case BackwardComposition:
				return new BackwardComposition(uc, lex, l, isHarmonic, t);
			case BackwardSubstitution:
				return new BackwardSubstitution(uc, lex, l, isHarmonic, t);	
			case ForwardComposition:
				return new ForwardComposition(uc, lex, l, isHarmonic, t);
			case ForwardSubstitution:
				return new ForwardSubstitution(uc, lex, l, isHarmonic, t);
			default:
				throw new NotImplementedException();
				
		}
	}
}
