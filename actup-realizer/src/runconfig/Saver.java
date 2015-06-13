package runconfig;

import java.util.List;
import java.util.concurrent.BlockingQueue;

import optimization.RunData;
import optimization.VariableSet;
import realize.Realization;
import evaluation.Evaluation;
import evaluation.Evaluator;

public class Saver {
	private long timeOfLastSave;
	private boolean manualsave;
	private Evaluator e;
	private final BlockingQueue<Message> result;
	public Saver(BlockingQueue<Message> result, IOSettings io) {
		timeOfLastSave = System.currentTimeMillis();
		try {
			e = Evaluator.instantiate(io);
		} catch (Exception e) {
			System.err.println("Unable to instantiate evaluator");
			System.exit(1);
		}
		this.result = result;
	}
	public void userSaving() {
		manualsave = true;
	}
	//only saves if needed
	public void handleSaving(String expName, List<Realization> realizations, List<Integer> files, VariableSet vars, long time) {
		if (manualsave) {
			manualsave = false;
			save(expName, realizations, files, vars, time);
		}
		else {
			long timems = timeOfLastSave - System.currentTimeMillis();
			long timeHours = timems / 3600000;
			if (timeHours >= 4) {
				save(expName, realizations, files, vars, time);				
			}
		}
	}
	//we also want to include the status as a manual part of a save.
	private void save(String expName, List<Realization> realizations, List<Integer> files, VariableSet vars, long time) {
		timeOfLastSave = System.currentTimeMillis();
		Evaluation ef = e.scoreAll(realizations);
		RunData rd = new RunData(files, vars, ef, expName, time);
		try {
			result.put(new ResultMessage(rd, expName));
		} catch (InterruptedException e1) {
			e1.printStackTrace();
			System.exit(1);
		}
	}

}
