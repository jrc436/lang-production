package grammar;

import lexicon.LexicalData;
import lexicon.Lexicon;
import lexicon.Tokenizer;
import synsem.Category;
import synsem.LF;
import unify.UnifyControl;

//a class created to allow post-hoc initialization of rules.
public class TypeChangingRuleStruct extends RuleStruct {
	public final Category arg;
	public final Category result;
	public final LF firstEP;
	public final String name;
	public TypeChangingRuleStruct(Lexicon l, Category arg, Category result, LF firstEP, String name, Tokenizer t) {
		super(RuleType.TypeChangingRule, l, t);
		this.arg = arg;
		this.result = result;
		this.firstEP = firstEP;
		this.name = name;
	}
	public Rule getRuleFromStruct(UnifyControl uc, LexicalData lex) {
		return new TypeChangingRule(uc, lex, l, this, t);
	}
}
