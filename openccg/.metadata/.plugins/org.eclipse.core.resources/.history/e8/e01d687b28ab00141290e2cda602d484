package ar.xmlparser;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LineParser {
	private final String regex;
	private final Pattern p;
	private final NodeHandler nh;
	int lineNumber;
	
	public LineParser(String regex) {
		this.regex = regex;
		this.p = Pattern.compile(regex);
		nh = new NodeHandler();
	}
	//XML 
	public LineParser() {
		this.regex = "^(?<start><[a-z\\-]+(?<attrs> [a-zA-z:0-9]+=\"[a-zA-Z0-9.'_:(),\\-\\[\\]]+\")*>)?(?<value>[a-zA-Z0-9?':!+`\\$\\-.,]+)?(?<close></[a-z\\-]+>)?$";
		this.p = Pattern.compile(regex);
		nh = new NodeHandler();
	}
	//note, due to unexpected behavior of group finding not recording all instances of a group in Java
	//this is sort of hacked together (adding up all the instances and then parsing them back)
	public boolean parse(String input) throws Exception {
		input = input.trim();				
		Matcher m = p.matcher(input);
		String[] vals = new String[4];
		while (m.find()) {
			vals[0] = m.group("start");
			vals[1] = m.group("attrs");
			vals[2] = m.group("value");
			vals[3] = m.group("close");
		}
		//since attrs repeats, it needs it's own special treatment
		//there are no attrs if there's no topic though!
		if (vals[0] != null) {
			String[] valsArr = vals[0].split(" "); //valsArr will be vals[0] and attrs that got added
			vals[0] = valsArr[0];
			for (int i = 1; i < valsArr.length; i++) {
				vals[1] += valsArr[i]; //out of order, but order doesn't matter
			}
		}
		
		//sanity check, a node should always be created or closed
		if (!NodeCreationRoutine(vals) && !NodeCloseRoutine(vals)) {
			//there are two options. There's a legitimate error, or it's a special node. So we will discard all "comment" nodes and "format" nodes
			try {
				if (!input.trim().equals("") && input.charAt(1) != '!' && input.charAt(1) != '?' &&  !input.trim().equals(System.getProperty("line.separator"))) {
					System.err.println(input);
				}	
			}
			catch (Exception e) {
				e.printStackTrace();
			}
			return false;
		}
		return true;
	}
	
	//simply for cleanliness
	private boolean NodeCreationRoutine(String[] vals) {
		if (vals[0] != null) {
			//if attr is null, then vals[0] will have the opening and closing bracket (not pulled off earlier)
			if (vals[1] == null) {
				vals[0] = vals[0].substring(1, vals[0].length()-1); 
			}
			//otherwise, just opening bracket
			else {
				vals[0] = vals[0].substring(1, vals[0].length()); 
			}
			XmlNode readingNode = new XmlNode(vals[0], vals[3], lineNumber);
			if (vals[1] != null) {
				String[] attrs = vals[1].trim().split(" "); //this should be all matches.
				for (String a : attrs) {
					String[] keyVal = a.trim().split("=");
					readingNode.addAttribute(keyVal[0], keyVal[1]);
				}
			}
			if (vals[3] == null) {
				nh.AddOpenNode(readingNode);
			}
			else {
				nh.AddClosedNode(readingNode);
			}
			return true;
		}
		return false;
	}
	
	//simply for cleanliness
	private boolean NodeCloseRoutine(String[] vals) throws Exception {
		//this means we just have an endtag.
		if (vals[0] == null && vals[3] != null) {
			//trimming off the closing slash and the brackets
			nh.CloseNode(vals[3].substring(2, vals[3].length()-1), lineNumber);
			return true;
		}
		return false;
	}
}
