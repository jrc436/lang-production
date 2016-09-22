package subredditanalysis.keywords;

import util.csv.SConfusionCSV;
import util.sys.Executor;
import util.wordmap.WordMap;

public class OriginDestinationExecutor extends Executor<OriginDestinationProcessor, WordMap, SConfusionCSV> {

	public OriginDestinationExecutor() {
		super("origdest", 40, OriginDestinationProcessor.class, WordMap.class, SConfusionCSV.class);
	}
	public static void main(String[] args) {
		OriginDestinationExecutor ode = new OriginDestinationExecutor();
		ode.initializeFromCmdLine(args);
		ode.run();
	}
	
}
