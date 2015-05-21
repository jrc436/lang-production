package grammar;

import lexicon.LexicalData;
import lexicon.Lexicon;
import lexicon.Tokenizer;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;
import unify.UnifyControl;

//as of now, actual rules rely on a specific run, because of their use of unify control. RuleStructs are the immutable data held in rulesets
//I get that the actual accepted paradigm for this is factories. I do not like factories. 14 May 2015 JRC
public class RuleStruct {
	protected final Tokenizer t;
	protected final Lexicon l;
	protected final RuleType rt;
	public RuleStruct(RuleType rt, Lexicon l, Tokenizer t) {
		this.rt = rt;
		this.l = l;
		this.t = t;
	}
	public Rule getRuleFromStruct(UnifyControl uc, LexicalData lex) {
		switch (rt) {
			case BackwardApplication:
				return new BackwardApplication(uc, lex, l, t);
			case BackwardComposition:
				return new BackwardComposition(uc, lex, l, t);
			case BackwardSubstitution:
				return new BackwardSubstitution(uc, lex, l, t);
			case ForwardApplication:
				return new ForwardApplication(uc, lex, l, t);
			case ForwardComposition:
				return new ForwardComposition(uc, lex, l, t);
			case ForwardSubstitution:
				return new ForwardSubstitution(uc, lex, l, t);
			default:
				throw new NotImplementedException();			
		}
	}
}
