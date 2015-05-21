package grammar;

//I hope this is temporary until a better solution to this type of couping can be found
//Marked temporary on 14 May 2015.
public enum RuleType {
	BackwardApplication,
	BackwardComposition,
	BackwardSubstitution,
	BackwardTypeRaising,
	ForwardApplication,
	ForwardComposition,
	ForwardSubstitution,
	ForwardTypeRaising,
	TypeChangingRule,
	FragmentJoining,
	GlueRule;
}
