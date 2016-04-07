package edu.psu.acs.lang;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import org.jactr.core.logging.Logger;
import org.jactr.core.logging.impl.DefaultModelLogger;
import org.jactr.core.model.IModel;
import org.jactr.core.production.IProduction;
import org.jactr.core.runtime.ACTRRuntime;

public class AttachLogger implements Runnable
{

  /**
   * called after all configurations are complete, but just before starting the models.
   */
  public void run()
  {
    ACTRRuntime.getRuntime().getModels().forEach((m)->installLogger(m));
    
  }
  private int poopNumberBackwards(String s) {
	  int poopedNum = -1;
	  for (int i = s.length()-1; i >= 0; i--) {
		  try {
			  poopedNum = Integer.parseInt(s.substring(i));
		  }
		  catch (NumberFormatException nfe) {
			  break;
		  }
	  }
	  return poopedNum;
  }
  private int extractResolveNum(String s) {
	  return poopNumberBackwards(s.split("-")[0]);
  }
  private int extractSyntaxRuleNum(String s) {
	  return extractResolveNum(s.split("--")[0]);
  }
  protected void initializeEarlyUtility(IModel model) {
	  Map<Integer, Integer> giftForIndex = new HashMap<Integer, Integer>();
	  int gifting = 100;
	  for (int i = 1; i <= 100; i++) {
		  giftForIndex.put(i, gifting);
		  gifting = gifting / 2;
	  }
	  giftForIndex.put(0, 0);
	  giftForIndex.put(-1, 0);
	  try {
		for (IProduction p : model.getProceduralModule().getProductions().get()) {
			int index = -1;
			if (p.toString().contains("--")) {
				//syntax rule
				index = extractSyntaxRuleNum(p.toString());
			}
			else if (p.toString().contains("-")) {
				//resolution rule
				index = extractResolveNum(p.toString());
			}
			else {
				//must be one of the others
				index = poopNumberBackwards(p.toString());
			}
			p.getSubsymbolicProduction().setParameter("Utility", ""+giftForIndex.get(index));
		}
		
	} catch (InterruptedException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	} catch (ExecutionException e) {
		// TODO Auto-generated catch block
		e.printStackTrace();
	}
  }
  protected void installLogger(IModel model)
  {
	//initializeEarlyUtility(model);
    DefaultModelLogger dml = new DefaultModelLogger();
//    //dml.setParameter("OUTPUT", "../../../../"+PathConsts.dataDirName+"/"+PathConsts.outputName);
    for (String s : dml.getSetableParameters()) {
    	dml.setParameter(s, s.toLowerCase()+".txt");
    }
    Logger.addLogger(model, dml);
  }
}