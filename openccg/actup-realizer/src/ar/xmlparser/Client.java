package ar.xmlparser;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Client {

	/**
	 * @param args
	 * @throws FileNotFoundException 
	 */
	public static void main(String[] args) throws FileNotFoundException {
		String regex = "^(<[a-z]+( [a-zA-z:0-9]+=\"[a-zA-Z0-9.'_:\\-\\[\\]]+\")*>)?([a-zA-Z0-9?':!+`\\$\\-.,]+)?(</[a-z\\-]+>)?$";
		String input = "<tag type=\"tok\">Forty-Seven</tag>";
		String inputt = "<rel arg1=\"[]:_G861010\">";
		String regext = "^<[a-z]+( [a-zA-z:0-9]+=\"[a-zA-Z0-9._:\\[\\]]+\")*>$";
		System.out.println(input.matches(regex));
		//String[] out = input.split(regex);
		Scanner in = new Scanner(new File("/home/jrc/ap-largefiles/switchboard_boxer.xml"));
		int lineNum = 0;
		while (in.hasNextLine()) {
			lineNum++;
			String line = in.nextLine().trim();
			if (!line.matches(regex)) {
				if (!(line.charAt(1) == '!')) {
					System.out.println(lineNum+": "+line);
				}
			}
		}
		in.close();
		
	}

}
