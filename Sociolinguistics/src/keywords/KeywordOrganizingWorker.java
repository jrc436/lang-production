package keywords;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.BlockingQueue;

import util.json.JsonLayer;
import util.json.JsonReadable;
import util.sys.Executable;

@Deprecated
public class KeywordOrganizingWorker extends Executable {
	private final JsonLayer jl;
	private final List<KeywordOrganizer> copyOfComments;
	public KeywordOrganizingWorker(int threadNum, BlockingQueue<String> log, JsonLayer jl, List<KeywordOrganizer> copyOfComments) {
		super(threadNum, log);
		this.jl = jl;
		this.copyOfComments = new ArrayList<KeywordOrganizer>(copyOfComments);
	}
	@Override
	public void run() {
		List<JsonReadable> comments = jl.getNextData();
		while (comments != null) {
			this.logMessage("Thread"+this.getNum()+" has acquired another set of comments");
			this.logMessage("There are "+jl.numReadableRemaining()+" files remaining");
			for (JsonReadable comment : comments) {
				for (KeywordOrganizer c : copyOfComments) {
					if (c.relevant(comment.get("body"))) {
						c.writeComment(comment.toString());
					}
				}
			}
			comments = jl.getNextData();
		}
		this.logMessage("Thread"+this.getNum()+ " will close now, as there are no more files to process");
	}
}