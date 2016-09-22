package armedconflicts;

import util.json.JsonKeyMap;
import util.json.JsonList;
import util.json.MergeData;
import util.sys.Executor;

public class ConflictMergeMain extends Executor<MergeData, JsonList, JsonKeyMap> {

	public ConflictMergeMain() {
		super("conflicts", 20, MergeData.class, JsonList.class, JsonKeyMap.class);
	}
	public static void main(String[] args) {
		ConflictMergeMain csv = new ConflictMergeMain();
		csv.initializeFromCmdLine(args);
		csv.run();
	}
//	private static final List<String> helpMsg = new ArrayList<String>();
//	private static final String comDirName = "arg0";
//	private static final String rdataCSV = "arg1";
//	private static final String rdataOut = "arg2";
//	private static final String yeardataCSV = "arg3";
//	//private static final String processBool = "arg4";
//	//private static final String processAddendum = "arg5";
//	private static final String processRegex = "arg4";
//	private static final char commaRep = '$';
//
//	private static void initHelp() {
//		helpMsg.add(comDirName + ": directory to comment files");
//		helpMsg.add(rdataCSV + ": path to CSV input file");
//		helpMsg.add(rdataOut + ": output path for the new CSV file");
//		helpMsg.add(yeardataCSV
//				+ ": the data with the year specific numbers for externalities");
//		helpMsg.add(processRegex
//				+ ": The regex files must match to be processed");
//		helpMsg.add(comDirName + "," + rdataCSV + "," + rdataOut + ","
//				+ yeardataCSV + " are always required");
//		helpMsg.add(processRegex + " is never required. Its default is to match all");
//	}
//
//	private static String help() {
//		String retval = "";
//		for (String s : helpMsg) {
//			retval += s + System.getProperty("line.separator");
//		}
//		return retval;
//	}
//
//	public static void main(String[] args) throws IOException {
//		initHelp();
//		
//		String match = null;
//		
//		if (args.length < 4) {
//			System.err.println("Required arguments not given");
//			System.err.println(help());
//			System.exit(1);
//		}
//		else if (args.length == 5) {
//			match = args[4];
//		}
//		int numRuns = ResourceAllocator.getSuggestedNumThreads(5);
//		
//		CSVReader csv = new CSVReader(Paths.get(args[1]), commaRep);		
//		
//		Map<String, ConflictDated> conflictdates = ConflictDated.getConflictDated(args[3], commaRep);
//		String[][] cells = csv.getCleanedCells();
//		JsonLayer jl = match == null ? new JsonLayer(Paths.get(args[0])) : new JsonLayer(Paths.get(args[0]), match);	
//		FileWriter fw = new FileWriter(args[2]);	
//		SentimentLayer sl = new SentimentLayer();
//		
//		ExecutorService es = Executors.newCachedThreadPool();
//		
//		fw.write("Conflict,Region,Criminal,Foreign,Terrorism,Territorial,Ethnic,Separatist,Age,Intensity,Casualities,IDP,Refugees,"
//				+ "Same-Year-Fatalities,Same-Year-Refugees,Same-Year-IDP,Score,Controversy,Sentiment,Author,Subreddit"+System.getProperty("line.separator"));
//		fw.flush();
//		
//		List<List<String>> conflictAssignments = new ArrayList<List<String>>();
//		List<List<Integer>> conflictNums = new ArrayList<List<Integer>>();
//		for (int i = 0; i < numRuns; i++ ) {
//			conflictAssignments.add(new ArrayList<String>());
//			conflictNums.add(new ArrayList<Integer>());
//		}
//		BlockingQueue<String> messages = new LinkedBlockingQueue<String>();
//		Thread log = new Thread(new Logger(messages, "log.txt"));
//		log.setDaemon(true);
//		log.start();
//		
//		
//		String[] conflictNames = csv.getColumnByTitle("Conflict");
//		System.out.println("Assigning "+conflictNames.length+" conflicts to "+numRuns+" threads");
//		for (int j = 0; j < conflictNames.length; ) {
//			for (int i = 0; i < numRuns; i++, j++) {
//				conflictAssignments.get(i).add(StringCleaner.sanitizeForFiles(conflictNames[j]));
//				conflictNums.get(i).add(j);
//			}
//		}
//		for (int i = 0; i < numRuns; i++) {
//			es.execute(new Worker(messages, i, sl, conflictdates, jl, cells, fw, conflictNums.get(i), conflictAssignments.get(i)));	
//		}
//		es.shutdown();
//		
//
//	}
//}
//
//class Worker implements Runnable {
//	private final SentimentLayer sl;
//	
//	private JsonLayer jl;
//	private final Calendar c;
//	private final FileWriter fw;
//	private final int threadNum;
//	private final List<String> fileConflict;
//	private final List<Integer> conflictNums;
//	private final String[][] cells;
//	private final Map<String, ConflictDated> conflictdateds;
//	private final BlockingQueue<String> log;
//	
//	public Worker(BlockingQueue<String> log, int threadNum, SentimentLayer sl, Map<String, ConflictDated> conflictdates, JsonLayer j, String[][] cells, FileWriter fw, List<Integer> conflictNum, List<String> fileConflict) {
//		this.log = log;
//		this.jl = j;
//		this.threadNum = threadNum;
//		this.fileConflict = fileConflict;
//		this.conflictNums = conflictNum;
//		this.sl = sl;
//		this.conflictdateds = conflictdates;
//		this.cells = cells;
//		c = Calendar.getInstance();
//		this.fw = fw;
//	}	
//
//
//	@Override
//	public void run() {
//		System.out.println("Thread: "+threadNum+" is beginning its full run. Its been assigned: "+fileConflict.size()+" conflicts");
//		for (int conflictNum = 0; conflictNum < fileConflict.size(); conflictNum++) {
//			log.offer("Thread: "+threadNum+" is preparing its run on new conflict"+conflictNum+": "+fileConflict.get(conflictNum));
//			TaskDataStruct tds = getNewConflict(conflictNum);
//			log.offer("Thread: "+threadNum+" is beginning a run on new conflict: "+conflictNum+": "+fileConflict.get(conflictNum));
//			for (int i = 0; i < tds.jr.size(); i++) {		
//				try {
//					log.offer("Thread: "+threadNum+" has: "+(tds.jr.size()-i)+" more to go on this conflict");					
//					String dat = tds.jr.get(i).get("created_utc");
//					// for whatever reason, created_utc has quotes!
//					long timestamp = Long.parseLong(dat.substring(1, dat.length() - 1));
//					Date d = new Date(timestamp * 1000);
//					c.setTime(d);
//					int year = c.get(Calendar.YEAR);
//					if (year < 2012) {
//						year = 2012; //this is just because all of the data is marked as 2012... but might be slightly earlier...
//					}
//					String line = tds.prefix;
//					line += tds.cd.getDataForYear(ConflictExternality.Fatalities, year) + ",";
//					line += tds.cd.getDataForYear(ConflictExternality.Refugees, year) + ",";
//					line += tds.cd.getDataForYear(ConflictExternality.IDP, year) + ",";
//					line += Integer.parseInt(tds.jr.get(i).get("score")) + ",";
//					line += Integer.parseInt(tds.jr.get(i).get("controversiality")) + ",";
//					log.offer("Thread: "+threadNum+" is beginning sentiment analysis");		
//					try {
//						line += sl.getOutput(tds.jr.get(i).get("body")) + ",";
//					}
//					catch (NullPointerException npe) {
//						line += "NULL,";
//					}
//					log.offer("Thread: "+threadNum+" has completed sentiment analysis");	
//					line += tds.jr.get(i).get("author") + ",";
//					line += tds.jr.get(i).get("subreddit");
//					log.offer("Thread: "+threadNum+" is waiting to acquire the writer");
//					synchronized (fw) {
//						fw.write(line+System.getProperty("line.separator"));
//						fw.flush();
//					}
//					log.offer("Thread: "+threadNum+" has written another comment");
//				}
//				catch (NumberFormatException e) {
//					System.err.println("Trouble parsing numbers from Json: "+ tds.jr.get(i).toString());
//				} 
//				catch (IOException e) {
//					e.printStackTrace();
//				}				
//			}
//			log.offer("Thread: "+threadNum+" has finished processing conflict"+conflictNums.get(conflictNum)+": "+fileConflict.get(conflictNum));
//		} 
//		System.out.println("Thread: "+threadNum+" is ending its run");
//	}
//	
//	private TaskDataStruct getNewConflict(int conflictWorkNum) {
//		String prefix = "";
//		String fName = fileConflict.get(conflictWorkNum);
//		int cNum = conflictNums.get(conflictWorkNum);
//		for (int f = 0; f < 13; f++) {
//			prefix += cells[cNum + 1][f] + ",";
//		}
//		List<JsonReadable> jrl = jl.getReadableByName(fName);
//		if (jrl == null) {
//			System.err.println("Could not find any readable file containing: "+ fName);
//			jrl = new ArrayList<JsonReadable>();
//		}
//		else if (jrl.size() == 0) {
//			System.err.println("No Jsons found at file path: "+ fName);
//		}
//		return new TaskDataStruct(conflictdateds.get(fName), jrl, prefix);
//	}
//	class TaskDataStruct {
//		public final ConflictDated cd;
//		public final List<JsonReadable> jr;
//		public final String prefix;
//		public TaskDataStruct(ConflictDated cd, List<JsonReadable> jr, String pref) {
//			this.cd = cd;
//			this.jr = jr;
//			this.prefix = pref;
//		}
//	}
}
