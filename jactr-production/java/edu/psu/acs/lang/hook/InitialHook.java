package edu.psu.acs.lang.hook;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import org.jactr.core.logging.Logger;
import org.jactr.core.logging.impl.DefaultModelLogger;
import org.jactr.core.model.IModel;
import org.jactr.core.model.six.update.UpdateCycleProcessor6;
import org.jactr.core.production.IProduction;
import org.jactr.core.runtime.ACTRRuntime;

import edu.psu.acs.lang.RunConsts;

public class InitialHook implements Runnable {

	/**
	 * called after all configurations are complete, but just before starting
	 * the models.
	 */
	public void run() {
		UtilityInitialization ui = RunConsts.UI;
		for (IModel m : ACTRRuntime.getRuntime().getModels()) {
			setCycler(m);
			initializeUtility(m, ui);
			installLogger(m, ui);
		}
	}
	private Map<Integer, Integer> getUtilityMap(int maxWordsPerSentence, UtilityInitialization ui) {
		Map<Integer, Integer> giftForIndex = new HashMap<Integer, Integer>();
		int gifting = 100;	
		switch (ui) {
			case DEFAULT:
				gifting = 0;
				for (int i = 1; i <= maxWordsPerSentence; i++) {
					giftForIndex.put(i, gifting);
					gifting = gifting / 2;
				}
				giftForIndex.put(0, 0);
				giftForIndex.put(-1, 0);
				break;
			case EARLY:
				for (int i = 1; i <= maxWordsPerSentence; i++) {
					giftForIndex.put(i, gifting);
					gifting = gifting / 2;
				}
				giftForIndex.put(0, 0);
				giftForIndex.put(-1, 0);
				break;
			case LATE:
				for (int i = maxWordsPerSentence; i >= 1; i--) {
					giftForIndex.put(i, gifting);
					gifting = gifting / 2;
				}
				giftForIndex.put(0, 0);
				giftForIndex.put(-1, 0);
				break;
			case MIDDLE:
				int move = 1;
				int center = (maxWordsPerSentence+1) / 2;
				giftForIndex.put(center, gifting);
				while (center + move <= maxWordsPerSentence || center - move > 0) {
					gifting /= 2;
					giftForIndex.put(center+move, gifting);
					giftForIndex.put(center-move, gifting);
					move++;
				}
				giftForIndex.put(0, 0);
				giftForIndex.put(-1, 0);
				break;
			default:
				break;
			
		}
		return giftForIndex;
	}

	protected void initializeUtility(IModel model, UtilityInitialization ui) {
		Map<Integer, Integer> map = getUtilityMap(RunConsts.maxLength, ui);
		try {
			for (IProduction p : model.getProceduralModule().getProductions().get()) {
				int index = -1;
				if (p.toString().contains("--")) {
					// syntax rule
					index = extractSyntaxRuleNum(p.toString());
				} else if (p.toString().contains("-")) {
					// resolution rule
					index = extractResolveNum(p.toString());
				} else {
					// must be one of the others
					index = poopNumberBackwards(p.toString());
				}
				p.getSubsymbolicProduction().setParameter("Utility", "" + map.get(index));
			}

		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	protected void setCycler(IModel model) {
		model.setCycleProcessor(new UpdateCycleProcessor6());
	}

	protected void installLogger(IModel model, UtilityInitialization ui) {
		// initializeEarlyUtility(model);
		DefaultModelLogger dml = new DefaultModelLogger();
		dml.setParameter("OUTPUT", RunConsts.getOutputFromRuns(ui));
		DefaultModelLogger other = new DefaultModelLogger();
		for (String s : dml.getSetableParameters()) {
			other.setParameter(s, s.toLowerCase() + ".txt");
		}
		Logger.addLogger(model, dml);
		Logger.addLogger(model, other);
	}

	private int poopNumberBackwards(String s) {
		int poopedNum = -1;
		for (int i = s.length() - 1; i >= 0; i--) {
			try {
				poopedNum = Integer.parseInt(s.substring(i));
			} catch (NumberFormatException nfe) {
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
}