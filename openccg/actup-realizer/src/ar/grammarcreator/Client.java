package ar.grammarcreator;

import java.io.File;
import java.util.Scanner;

import ar.xmlparser.LineParser;
import ar.xmlparser.XmlNode;

public class Client {

	/**
	 * @param args
	 * @throws Exception 
	 */
	public static void main(String[] args) throws Exception {
		Scanner in = new Scanner(new File("/home/jrc/ap-largefiles/switchboard_boxer.xml"));		
		LineParser lp = new LineParser();
		
		int count = 0;
		long curTime = 0;
		long timeDiff = 0;
		long startTime = System.nanoTime();
		
		while (in.hasNextLine()) {
			count++;
			lp.lineNumber = count;
			lp.parse(in.nextLine());
			if (count % 1000000 == 0) {
				curTime = System.nanoTime();
				timeDiff = (curTime - startTime)/ (long)Math.pow(10, 9);
				System.out.println(count + ":"+timeDiff+"; ");
				startTime = curTime;
			}
		}
		in.close();
		
		String dirPath = "/home/jrc/actup-production/openccg/grammars/switchboard";
		
		File newDir = new File(dirPath);
		if (!newDir.exists()) {
			newDir.mkdir();
		}
		
		XmlNode root = lp.getTopLevelNode();
		LexiconBuild lb = new LexiconBuild();
		MorphBuild mb = new MorphBuild(dirPath, root);
		mb.output();
	
	}
}
