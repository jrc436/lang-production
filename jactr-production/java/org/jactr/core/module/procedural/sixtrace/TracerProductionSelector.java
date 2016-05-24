package org.jactr.core.module.procedural.sixtrace;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jactr.core.module.procedural.IProceduralModule;
import org.jactr.core.module.procedural.IProductionSelector;
import org.jactr.core.module.procedural.six.DefaultProductionSelector;
import org.jactr.core.module.procedural.six.learning.DefaultProceduralLearningModule6;
import org.jactr.core.production.IInstantiation;
import org.jactr.core.production.VariableBindings;
import org.jactr.core.production.six.ISubsymbolicProduction6;

import edu.psu.acs.lang.RunConsts;
import edu.psu.acs.lang.declarative.CCGOperator;
import edu.psu.acs.lang.declarative.CCGOperatorEnum;
import edu.psu.acs.lang.production.SyntaxRuleType;
import edu.psu.acs.lang.tracing.GraphMaker;
import edu.psu.acs.lang.tracing.IDependencyGraph;
import edu.psu.acs.lang.tracing.IDependencyGraph.IDependencyNode;
import edu.psu.acs.lang.util.NodeParser;
import edu.psu.acs.lang.util.ParseException;
import edu.psu.acs.lang.util.RuleNode;

public class TracerProductionSelector implements IProductionSelector {
	private List<IDependencyGraph<RuleNode>> fireChecker;
	private FileWriter log;
	public TracerProductionSelector() throws ParseException {
		Path workingDir = Paths.get(RunConsts.workingDir);
		Path dataDir = workingDir.resolve(RunConsts.dataDirName);
		Path expDir = dataDir.resolve(RunConsts.expName);
		Path ccgPath = expDir.resolve(RunConsts.typeCat);
		NodeParser n = new NodeParser(ccgPath, false);
		fireChecker = GraphMaker.make(n);
		try {
			log = new FileWriter(expDir.resolve("log.txt").toFile());
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
	}
	 /**
	   * Logger definition
	   */
	  static private final transient Log LOGGER = LogFactory.getLog(DefaultProductionSelector.class);

	  private IProceduralModule _module;

	  public IInstantiation select(Collection<IInstantiation> instantiations) {
		  for (IInstantiation in : instantiations) {
			  String prodName = in.getProduction().getSymbolicProduction().getName(); //we can figure out the rule from the production name.
			  SyntaxRuleType toCompare = null;
			  if (prodName.contains(SyntaxRuleType.ForwardApplication.toString())) {
				  toCompare = SyntaxRuleType.ForwardApplication;
			  }
			  else if (prodName.contains(SyntaxRuleType.BackwardApplication.toString())) {
				  toCompare = SyntaxRuleType.BackwardApplication;		
			  }
			  else if (prodName.contains(SyntaxRuleType.BackwardComposition.toString())) {
				  toCompare = SyntaxRuleType.BackwardComposition;
			  }
			  else if (prodName.contains(SyntaxRuleType.ForwardComposition.toString())) {
				  toCompare = SyntaxRuleType.ForwardComposition;
			  }
			  else if (prodName.contains(SyntaxRuleType.CONJDL.toString())) {
				  toCompare = SyntaxRuleType.CONJDL;
			  }
			  else if (prodName.contains(SyntaxRuleType.CONJDR.toString())) {
				  toCompare = SyntaxRuleType.CONJDR;
			  }
			  else if (prodName.contains(SyntaxRuleType.CONJL.toString())) {
				  toCompare = SyntaxRuleType.CONJL;
			  }
			  else if (prodName.contains(SyntaxRuleType.CONJR.toString())) {
				  toCompare = SyntaxRuleType.CONJR;
			  }
			  else {
				  return in;
			  }
			  if (prodName.contains("Resolve")) {
				  return in;
			  };
			  VariableBindings vb = in.getVariableBindings();
			  //		try {
			  //			FileWriter fw = new FileWriter("../vb.txt");
			  //			fw.write("here we are!\n");
			  //			Set<String> names = vb.getVariables();
			  //			for (String name : names) {
			  //				fw.write(name + ":-:" + vb.get(name) +"\n");
			  //			}
			  //			fw.close();
			  //		} catch (IOException e) {
			  //			// TODO Auto-generated catch block
			  //			e.printStackTrace();
			  //		}
			  String lword = ""+vb.get("=lword");
			  String rword = ""+vb.get("=rword");
			  Object[] validators = new Object[5];
			  validators[2] = lword;
			  validators[3] = rword;
			  validators[4] = toCompare;
			  String aType = ""+vb.get("=a");
			  String bType = ""+vb.get("=b");
			  String cType = ""+vb.get("=c"); //depends on the rule...
			  switch (toCompare) {			
				  case BackwardApplication:
					  validators[0] = bType;
					  validators[1] = aType + new CCGOperator(CCGOperatorEnum.Backslash).toString() + bType;
					  break;
				  case BackwardComposition:
					  validators[0] = aType + new CCGOperator(CCGOperatorEnum.Backslash).toString() + bType;
					  validators[1] = cType + new CCGOperator(CCGOperatorEnum.Backslash).toString() + aType;
					  break;
				  case ForwardApplication:
					  validators[1] = bType;
					  validators[0] = aType + new CCGOperator(CCGOperatorEnum.Slash).toString() + bType;
					  break;
				  case ForwardComposition:
					  validators[0] = aType + new CCGOperator(CCGOperatorEnum.Slash).toString() + bType; 
					  validators[1] =	bType + new CCGOperator(CCGOperatorEnum.Slash).toString() + cType;
					  break;
				  default:
					  break;
			  }
			  //		Pattern pattern = Pattern.compile(toCompare.toString() + "([0-9]+)\\-([0-9]+)\\-\\-([0-9]+)\\-([0-9]+)");
			  //		Matcher patMatch = pattern.matcher(prodName);
			  //		if (!patMatch.matches()) {
			  //			System.err.println(prodName + " does not match pattern: "+pattern);
			  //			System.err.println("It is possible the formatting of rule names has changed, if so, please update pattern accordingly.");
			  //			System.exit(1);
			  //		}
			  //		String leftWordNum = patMatch.group(0);
			  //		String leftTypeNum = patMatch.group(1);
			  //		String rightWordNum = patMatch.group(2);
			  //		String rightWord = patMatch.group(3); //not sure if we need this or not
			  String goalName = ""+vb.get("=goal");
			  
			  int goalNum = Integer.parseInt(goalName.substring(4));
			  IDependencyNode<RuleNode> node = fireChecker.get(goalNum-1).getFreeEquivNode(validators);
			  //to know which goal we are checking against, we need the name of the current goal buffer. Then we can simply get the dependencygraph_i
			  try {
				  if (node == null) {
					  //do something bad
				//	  ((ISubsymbolicProduction6)in.getProduction().getSubsymbolicProduction()).setReward(DefaultProceduralLearningModule6.SKIP_REWARD);
					  log.write(prodName +" failed to fire because it wasn't valid.\n");
				  }
				  else {
					  fireChecker.get(goalNum).popNode(node);
				//	  ((ISubsymbolicProduction6)in.getProduction().getSubsymbolicProduction()).setReward(DefaultProceduralLearningModule6.PARTICIPATE);
					  log.write(prodName +" fired.\n");
					  return in;
				  }
			  }
			  catch (IOException e) {
				  e.printStackTrace();
				  System.exit(1);
			  }
			  //pme.fire(this);
			  //		Collection<ICondition> conds = pme.getProduction().getSymbolicProduction().getConditions();
			  //		for (ICondition ic : conds) {
			  //			if (!(ic instanceof AbstractSlotCondition)) {
			  //				continue;
			  //			}
			  //			AbstractSlotCondition asc = (AbstractSlotCondition) ic;
			  //			Collection<? extends org.jactr.core.slot.ISlot> slots = asc.getSlots();
			  //			for (org.jactr.core.slot.ISlot slot : slots) {
			  //				slot.getName();
			  //				slot.getValue();
			  //				
			  //			}
			  //		}
			  //		((ISubsymbolicProduction6)pme.getProduction().getSubsymbolicProduction()).setReward(-1);
		  }
		  return null;
	  }

	  public void setProceduralModule(IProceduralModule module)
	  {
	    _module = module;
	  }

	  public IProceduralModule getProceduralModule()
	  {
	    return _module;
	  }
}
