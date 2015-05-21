package grammar;

import lexicon.LexicalData;
import lexicon.Lexicon;
import lexicon.Tokenizer;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;
import synsem.Category;
import unify.UnifyControl;

public class TypeRaisingRuleStruct extends RuleStruct {

	private final Category arg;
	private final Category result;
	private final boolean addDollar;
	public TypeRaisingRuleStruct(RuleType rt, Lexicon l, boolean addDollar, Category arg, Category result, Tokenizer t) {
		super(rt, l, t);
		this.addDollar = addDollar;
		this.arg = arg;
		this.result = result;
	}
	
	public Rule getRuleFromStruct(UnifyControl uc, LexicalData lex) {
		switch (rt) {
			case BackwardTypeRaising:
				return new BackwardTypeRaising(uc, lex, l, addDollar, arg, result, t);		
			case ForwardTypeRaising:
				return new ForwardTypeRaising(uc, lex, l, addDollar, arg, result, t);
			default:
				throw new NotImplementedException();
			
		}
	}
	
}
