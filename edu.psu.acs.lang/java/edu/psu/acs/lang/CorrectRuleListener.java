//package edu.psu.acs.lang;
//
//import java.io.File;
//import java.net.URISyntaxException;
//import java.nio.file.Path;
//import java.util.List;
//
//import org.jactr.core.concurrent.ExecutorServices;
//import org.jactr.core.event.IParameterEvent;
//import org.jactr.core.model.IModel;
//import org.jactr.core.module.procedural.event.IProceduralModuleListener;
//import org.jactr.core.module.procedural.event.ProceduralModuleEvent;
//import org.jactr.core.module.procedural.six.learning.DefaultProceduralLearningModule6;
//import org.jactr.core.production.IInstantiation;
//import org.jactr.core.production.VariableBindings;
//import org.jactr.core.production.six.ISubsymbolicProduction6;
//import org.jactr.instrument.IInstrument;
//
//import edu.psu.acs.lang.IDependencyGraph.IDependencyNode;
//import edu.psu.acs.lang.declarative.CCGOperator;
//import edu.psu.acs.lang.declarative.CCGOperatorEnum;
//import edu.psu.acs.lang.production.SyntaxRuleType;
//
//public class CorrectRuleListener implements IInstrument {
//	
//	public static Path getCCGCatPath() {
//		try {
//			File workingDir = new File(CorrectRuleListener.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());
//			Path basePath = workingDir.getParentFile().toPath();
//			Path dataDir = basePath.resolve(PathConsts.dataDirName);
//			Path expDir = dataDir.resolve(PathConsts.expName);
//			return expDir.resolve(PathConsts.typeCat);
//		}
//		catch (URISyntaxException e) {
//			System.err.println("Was unable to get the path to the current execution directory.");
//			System.exit(1);
//		}
//		return null;
//	}
//	class Listener implements IProceduralModuleListener {
//		@Override
//		public void parameterChanged(IParameterEvent pe) {
//			// TODO Auto-generated method stub
//		}
//
//		@Override
//		public void productionAdded(ProceduralModuleEvent pme) {
//			// TODO Auto-generated method stub
//			
//		}
//
//		@Override
//		public void conflictSetAssembled(ProceduralModuleEvent pme) {
//			// TODO Auto-generated method stub
//			
//		}
//
//		@Override
//		public void productionWillFire(ProceduralModuleEvent pme) {
//			String prodName = pme.getProduction().getSymbolicProduction().getName(); //we can figure out the rule from the production name.
//			SyntaxRuleType toCompare = null;
//			if (prodName.contains(SyntaxRuleType.ForwardApplication.toString())) {
//				toCompare = SyntaxRuleType.ForwardApplication;
//			}
//			else if (prodName.contains(SyntaxRuleType.BackwardApplication.toString())) {
//				toCompare = SyntaxRuleType.BackwardApplication;		
//			}
//			else if (prodName.contains(SyntaxRuleType.BackwardComposition.toString())) {
//				toCompare = SyntaxRuleType.BackwardComposition;
//			}
//			else if (prodName.contains(SyntaxRuleType.ForwardComposition.toString())) {
//				toCompare = SyntaxRuleType.ForwardComposition;
//			}
//			else {
//				return;
//			}
//			if (prodName.contains("Resolve")) {
//				return;
//			}
//			IInstantiation inst = (IInstantiation) pme.getProduction();
//			VariableBindings vb = inst.getVariableBindings();
////			try {
////				FileWriter fw = new FileWriter("../vb.txt");
////				fw.write("here we are!\n");
////				Set<String> names = vb.getVariables();
////				for (String name : names) {
////					fw.write(name + ":-:" + vb.get(name) +"\n");
////				}
////				fw.close();
////			} catch (IOException e) {
////				// TODO Auto-generated catch block
////				e.printStackTrace();
////			}
//			String lword = ""+vb.get("lword");
//			String rword = ""+vb.get("rword");
//			Object[] validators = new Object[5];
//			validators[2] = lword;
//			validators[3] = rword;
//			validators[4] = toCompare;
//			String aType = ""+vb.get("A");
//			String bType = ""+vb.get("B");
//			String cType = ""+vb.get("C"); //depends on the rule...
//			switch (toCompare) {			
//				case BackwardApplication:
//					validators[0] = bType;
//					validators[1] = aType + new CCGOperator(CCGOperatorEnum.Backslash).toString() + bType;
//					break;
//				case BackwardComposition:
//					validators[0] = aType + new CCGOperator(CCGOperatorEnum.Backslash).toString() + bType;
//					validators[1] = cType + new CCGOperator(CCGOperatorEnum.Backslash).toString() + aType;
//					break;
//				case ForwardApplication:
//					validators[1] = bType;
//					validators[0] = aType + new CCGOperator(CCGOperatorEnum.Slash).toString() + bType;
//					break;
//				case ForwardComposition:
//					validators[0] = aType + new CCGOperator(CCGOperatorEnum.Slash).toString() + bType; 
//					validators[1] =	bType + new CCGOperator(CCGOperatorEnum.Slash).toString() + cType;
//					break;
//				default:
//					break;
//			}
////			Pattern pattern = Pattern.compile(toCompare.toString() + "([0-9]+)\\-([0-9]+)\\-\\-([0-9]+)\\-([0-9]+)");
////			Matcher patMatch = pattern.matcher(prodName);
////			if (!patMatch.matches()) {
////				System.err.println(prodName + " does not match pattern: "+pattern);
////				System.err.println("It is possible the formatting of rule names has changed, if so, please update pattern accordingly.");
////				System.exit(1);
////			}
////			String leftWordNum = patMatch.group(0);
////			String leftTypeNum = patMatch.group(1);
////			String rightWordNum = patMatch.group(2);
////			String rightWord = patMatch.group(3); //not sure if we need this or not
//			String goalName = ""+vb.get("A");
//			int goalNum = Integer.parseInt(goalName.substring(4));
//			IDependencyNode<RuleNode> node = fireChecker.get(goalNum).getFreeEquivNode(validators);
//			//to know which goal we are checking against, we need the name of the current goal buffer. Then we can simply get the dependencygraph_i
//			if (node == null) {
//				//do something bad
//				((ISubsymbolicProduction6)pme.getProduction().getSubsymbolicProduction()).setReward(DefaultProceduralLearningModule6.SKIP_REWARD);
//			}
//			else {
//				fireChecker.get(goalNum).popNode(node);
//				((ISubsymbolicProduction6)pme.getProduction().getSubsymbolicProduction()).setReward(DefaultProceduralLearningModule6.PARTICIPATE);
//			}
//			//pme.fire(this);
////			Collection<ICondition> conds = pme.getProduction().getSymbolicProduction().getConditions();
////			for (ICondition ic : conds) {
////				if (!(ic instanceof AbstractSlotCondition)) {
////					continue;
////				}
////				AbstractSlotCondition asc = (AbstractSlotCondition) ic;
////				Collection<? extends org.jactr.core.slot.ISlot> slots = asc.getSlots();
////				for (org.jactr.core.slot.ISlot slot : slots) {
////					slot.getName();
////					slot.getValue();
////					
////				}
////			}
////			((ISubsymbolicProduction6)pme.getProduction().getSubsymbolicProduction()).setReward(-1);
//		}
//
//		@Override
//		public void productionFired(ProceduralModuleEvent pme) {
//			// TODO Auto-generated method stub
//			//should also add it so that the production glues words together!
//		}
//
//		@Override
//		public void productionCreated(ProceduralModuleEvent pme) {
//			// TODO Auto-generated method stub
//			
//		}
//
//		@Override
//		public void productionsMerged(ProceduralModuleEvent pme) {
//			// TODO Auto-generated method stub
//			
//		}
//	}
//	private List<IDependencyGraph<RuleNode>> fireChecker;
//	private Listener listen = new Listener();
//
//	@Override
//	public void install(IModel model) {
//		// TODO Auto-generated method stub
//		System.out.println("installing!");
//		NodeParser n = new NodeParser(getCCGCatPath());
//		fireChecker = GraphMaker.make(n);
//		model.getProceduralModule().addListener(listen, ExecutorServices.INLINE_EXECUTOR);
//		
//	}
//	@Override
//	public void uninstall(IModel model) {
//		// TODO Auto-generated method stub
//		
//	}
//	@Override
//	public void initialize() {
//		// TODO Auto-generated method stub
//		
//	}
//
//}
