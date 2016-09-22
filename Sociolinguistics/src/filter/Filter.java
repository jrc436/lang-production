package filter;

import java.util.ArrayList;
import java.util.List;

import util.sys.DataType;

public abstract class Filter {
	public abstract void filter(DataType dt);
//	public void filter(String fileIn, String fileOut, BlockingQueue<String> messages) {
//		FileInputStream fr = null;
//		FileWriter fw = null;
//		messages.offer("Beginning filter process. End file will be available at: "+fileOut);
//		try {
//			fr = new FileInputStream(fileIn);
//			fw = new FileWriter(fileOut);
//		}
//		catch (IOException ie) {
//			System.err.println("Error with input");
//			ie.printStackTrace();
//			System.exit(1);
//		}
//		Scanner scan = new Scanner(fr);
//		filterCritical(scan, fw, messages);
//		messages.offer("Filter ending");
//	}
//	protected abstract void filterCritical(Scanner s, FileWriter fw, BlockingQueue<String> log);
	private static Filter getFilterByName(String name) {
		return FilterEnum.instantiate(name);
	}
	public static Filter getFilter(String[] filters) {
		if (filters.length == 1) {
			return getFilterByName(filters[0]);
		}
		List<Filter> cat = new ArrayList<Filter>();
		for (String s : filters) {
			Filter f = getFilterByName(s);
			if (f != null) {
				cat.add(f);
			}
		}
		return new Filter() {
			@Override
			public void filter(DataType dt) {
				for (Filter f : cat) {
					f.filter(dt);
				}
			}
//			@Override
//			public void filterCritical(Scanner s, FileWriter fw, BlockingQueue<String> log) {
//				//String curFileIn = fileIn;
//				//String curFileOut = fileOut;
//				FileWriter curFileWriter = null;
//				File tmpFile = null;
//				try {
//					for (int i = 0; i < cat.size(); i++) {
//						if (i != cat.size()-1) {
//							//ok need to write to an intermediary fileout
//							String fpath = "filter"+i+".tmp";
//							tmpFile = new File(fpath);
//							curFileWriter = new FileWriter(tmpFile);
//							log.offer("Starting filter: "+i+" intermediate file will be available at: "+fpath);	
//						}
//						else {
//							//okay, time to write to the final fileout
//							curFileWriter = fw;
//							log.offer("Starting final pass filter.");
//						}
//						cat.get(i).filterCritical(s, curFileWriter, log);
//						log.offer("Filter: "+i+" is completed.");
//						s = new Scanner(new FileInputStream(tmpFile));
//					}
//				}
//				catch (IOException ie) {
//					System.err.println("Error in chaining filters. This is a bug that shouldn't ever happen.");
//					ie.printStackTrace();
//					System.exit(1);
//				}
//			}
		};
	}
	public static String getKnownFilters() {
		String toReturn = "";
		for (FilterEnum fe : FilterEnum.values()) {
			toReturn += fe.toString() + "; ";
		}
		return toReturn + " Any fully qualified class name extending wordtracer.Filter";
	}
	enum FilterEnum {
		lex,
		freq;
		public String toString() {
			switch (this) {
				case freq:
					return "Frequency";
				case lex:
					return "Lexical";	
				default:
					return "NULL";		
			}
		}
		static FilterEnum getEnumFromName(String name) {
			for (FilterEnum fe : FilterEnum.values()) {
				if (name.equals(fe.toString())) {
					return fe;
				}
			}
			return null;
		}
		static Filter instantiate(String name) {
			FilterEnum fe = getEnumFromName(name);
			if (fe != null) {
				return fe.instantiate();
			}		
			try {
				Class<?> cls = Class.forName(name);
				if (cls.isAssignableFrom(Filter.class)) {
					System.err.println(name + " is a class, but is not a Filter");
			//		System.exit(1);
				}
				Class<? extends Filter> clsl = cls.asSubclass(Filter.class);
				return clsl.newInstance();
			}
			catch (ClassNotFoundException c) {
				System.err.println(name+" does not refer to a loaded class");
				System.err.println("Allowable: " + getKnownFilters());
				c.printStackTrace();
			//	System.exit(1);
			} catch (InstantiationException e) {
				e.printStackTrace();
				System.err.println("Error creating instance of Filter: "+name+". Perhaps no default constructor is provided?");
			//	System.exit(1);
			} catch (IllegalAccessException e) {
				e.printStackTrace();
				System.err.println("Error creating instance of Filter: "+name+". Perhaps no public or protected constructor is provided?");
			//	System.exit(1);
			}
			return null;
		}
		Filter instantiate() {
			switch (this) {
				case freq:
					return new FreqFilter();
				case lex:
					return new LexFilter();
				default:
					return null;
				}
		}
	}
}
