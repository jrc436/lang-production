//package worddata;
//
//import java.util.List;
//import java.util.concurrent.BlockingQueue;
//
//import util.data.RedditComment;
//import util.json.JsonLayer;
//import util.json.JsonReadable;
//import util.sys.Executable;
//import util.wordmap.WordMap;
//
//@Deprecated
//class WordDataWorker extends Executable {
//	private final JsonLayer jl;
//	private final WordMap joinedMap;
//	private final WordMap personalMap;
//	public WordDataWorker(int threadNum, BlockingQueue<String> log, JsonLayer jl, WordMap full, WordMap personal) {
//		super(threadNum, log);
//		this.jl = jl;
//		this.joinedMap = full;
//		this.personalMap = personal;
//	}
//	public String call() {
//		run();
//		return "";
//	}
//	@Override
//	public void run() {
//		this.logMessage("Thread"+getNum()+" is beginning its run");
//		while (true) {
//			this.logMessage("Thread"+getNum()+" is waiting to acquire another file. "+jl.numReadableRemaining()+ " files remain.");
//			List<JsonReadable> jsons = jl.getNextData();			
//			if (jsons == null) {
//				this.logMessage("Thread"+getNum()+" has failed to find another list. It's ending its run.");
//				break;
//			}
//			else {
//				this.logMessage("Thread"+getNum()+" has acquired another list of jsons");		
//			}
//			for (JsonReadable j : jsons) {
//				RedditComment poop = new RedditComment(j);
//				personalMap.addComment(poop);
//			}
//		}
//		this.logMessage("Thread"+getNum()+" is beginning its combination.");
//		joinedMap.combine(personalMap);
//		this.logMessage("Thread"+getNum()+" is exiting.");
//	}		 
//}
