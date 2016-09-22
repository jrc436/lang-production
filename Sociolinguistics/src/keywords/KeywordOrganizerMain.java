package keywords;

import util.json.JsonList;
import util.listdata.KeywordList;
import util.sys.Executor;


public class KeywordOrganizerMain extends Executor<KeywordMatchProcessor, JsonList, KeywordList> {
	private static final String name = "keywords";
	private static final int gbPerThread = 40;
	public KeywordOrganizerMain() {
		super(name, gbPerThread, KeywordMatchProcessor.class, JsonList.class, KeywordList.class);
	}

	public static void main(String args[]) {
		KeywordOrganizerMain fm = new KeywordOrganizerMain();
		fm.initializeFromCmdLine(args);
		fm.run();
	}
}
//	private static final String name = "comment-organizer";
//	private static final int memReq = 4;
//	private final JsonLayer jl;
//	private final List<KeywordOrganizer> keywords;
//	public CommentOrganizer(JsonLayer jl, List<KeywordOrganizer> keywords) {
//		super(name, memReq);
//		this.jl = jl;
//		this.keywords = keywords;
//	}
//	@Override
//	public Executable getExecutable(int threadNum, BlockingQueue<String> messages) {
//		return new KeywordOrganizingWorker(threadNum, messages, jl, new ArrayList<KeywordOrganizer>(keywords));
//	}
//
//	public static void main(String[] args) {
//		//args[0] will be the input directory
//		//args[1] will be the output directory
//		//args[2] will be the path to the csvfile
//		//args[3] will be the type of csvfile
//		List<KeywordOrganizer> keywords = null;
//		switch (CsvType.fromString(args[3])) {
//			case conflicts:
//				keywords = initializeConflicts(Paths.get(args[2]), Paths.get(args[1]));
//				break;
//			case simple:
//				keywords = simpleInitialize(Paths.get(args[2]), Paths.get(args[1]));
//				break;
//			case wordmap:
//				keywords = wordmapInitialize(Paths.get(args[2]), Paths.get(args[1]));
//				break;
//			default:
//				System.err.println("This code should be unreachable!");
//				break;	
//		}
//		JsonLayer jl = new JsonLayer(Paths.get(args[0]));
//		CommentOrganizer co = new CommentOrganizer(jl, keywords);
//		co.run();
//	}
//	
//	private static List<KeywordOrganizer> wordmapInitialize(Path listfile, Path outDir) {
//		try {
//			List<String> lines = Files.readAllLines(listfile);
//			String header = lines.remove(0);
//			WordMap wm = new WordMap(header);
//			for (String s : lines) {
//				wm.addFromString(s);
//			}
//			return simpleHelp(wm.keySet(), outDir);
//		} catch (IOException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//		return null;
//	}
//	private static List<KeywordOrganizer> simpleHelp(Collection<String> lines, Path outDir) throws IOException {
//		List<KeywordOrganizer> keywords = new ArrayList<KeywordOrganizer>();
//		for (String line : lines) {
//			keywords.add(new KeywordOrganizer(outDir, StringCleaner.sanitizeForFiles(line+".txt"), line));
//		}
//		return keywords;
//	}
//	private static List<KeywordOrganizer> simpleInitialize(Path listFile, Path outDir) {
//		try {
//			return simpleHelp(Files.readAllLines(listFile), outDir);
//		} catch (IOException e) {
//			e.printStackTrace();
//			System.err.println("bad keyword list file path: "+listFile);
//			System.exit(1);
//		}
//		return null;
//	}
//	private static List<KeywordOrganizer> initializeConflicts(Path csvFile, Path outDir) {
//		CSVReader csv = new CSVReader(csvFile, '$');
//		List<KeywordOrganizer> conflicts = new ArrayList<KeywordOrganizer>();
//		String[] conflictNames = csv.getColumnByTitle("Conflict/Country Name");
//		String[] keywords = csv.getColumnByTitle("Alias/Keywords");
//		for (int i = 0; i < conflictNames.length; i++) {
//			try {
//				conflicts.add(new KeywordOrganizer(outDir, StringCleaner.sanitizeForFiles(conflictNames[i])+".txt", keywords[i].split(",")));
//			} catch (IOException e) {
//				System.err.println("Conflict: "+conflictNames[i]+" failed to initialize");
//				e.printStackTrace();
//			}
//		}
//		return conflicts;
//	}
