package ar.xmlparser;

import java.util.Scanner;

public class LineParser {
	private String nodeType;
	private String nodeValue;
	private boolean nodeClosed;
	public void parse(String line) throws Exception {
		line.trim();
		Scanner s = new Scanner(line);
		String iter = "start";
		
		//should do this with regex.
		//an xmlNode is "^<[a-z]+(?<attrs> [a-z]+="[a-z]+")*>(?<value>[a-z])?(?<closetag>+</[a-z]+>)?$"
		
		this.nodeType = "";
		this.nodeValue = "";
		
		while (s.hasNext()) {
			String cur = s.next();
			switch(iter) {
				case "start":
					if (cur.charAt(0) != '<') {
						throw new Exception("Format error. Line without leading '<'");
					}
					//should be able to grab the node "type" right away
					if (cur.charAt(cur.length() - 1) == '>') {
						this.nodeType = cur.substring(1, cur.length()-1);
						//if the while loop ends, it's an "open node", if it doesn't, it's going to grab the value 
					}
					else {
						this.nodeType = cur.substring(1, cur.length());
						iter = "attrs";
					}
				default:
			}
		}
		
		
		s.close();
	}
	public boolean getNodeClosed() {
		return nodeClosed;
	}
	public String getNodeType() {
		return nodeType;
	}
	public String getNodeValue() {
		return nodeValue;
	}
}
