package ar.grammarcreator;

import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Stack;

import ar.xmlparser.XmlNode;


//takes the list of nodes, makes some grammar files.
//every xdrs in the file represents one sentence
//every tagtoken represents a word
//the merged part represents the actual semantics, but is fairly difficult to figure out
//technically, the only "toplevel" node is xdrs-output

public class MorphBuild {
	private final ArrayList<String> header;
	private final ArrayList<Entry> entryWords;
	private final XmlNode root;
	private final String outPath;
	private final String constPath = "morph.xml";
	public MorphBuild(String outPath, XmlNode root) {
		header = new ArrayList<String>();
		header.add("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
		header.add("<morph xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"../morph.xsd\" name=\"English\">");
		entryWords = new ArrayList<Entry>();
		this.root = root;
		this.outPath = outPath+"/"+constPath;
	}
	//push it all to a file
	public void output() throws Exception {
		parse();
		FileWriter fw = new FileWriter(this.outPath);
		for (String h : header) {
			fw.write(h);
			fw.write(System.getProperty("line.separator"));
		}
		for (Entry e : entryWords) {
			fw.write("<entry word=\""+e.getWord()+" ");
			fw.write("pos=\""+e.getPos());
			if (!e.getStem().equals("")) {
				fw.write(" ");
				fw.write("stem=\""+e.getStem()+"\"");
			}
			if (!e.getMacros().equals("")) {
				fw.write(" ");
				fw.write("macros=\""+e.getMacros()+"\"");
			}
			fw.write("/>");
			fw.write(System.getProperty("line.separator"));
		}
		fw.close();
	}
	//take tree root and populate entryWords
	private void parse() throws Exception {
		Stack<XmlNode> s = new Stack<XmlNode>();
		s.push(root);
		while (!s.isEmpty()) {
			XmlNode cur = s.pop();
			for (XmlNode child : cur.getChildren()) {
				s.push(child);
			}
			this.process(cur);
		}
	}
	private void process(XmlNode x) throws Exception {
		if (x.getLabel().equals("tags")) {
			ArrayList<XmlNode> children = x.getChildren();
			Entry e = null;
			//data validation
			if (children.get(0).getLabel().equals("tag") && children.get(0).getAttribute("type").equals("tok") &&
				children.get(1).getLabel().equals("tag") && children.get(1).getAttribute("type").equals("pos") &&
				children.get(2).getLabel().equals("tag") && children.get(0).getAttribute("type").equals("lemma") &&
				children.get(3).getLabel().equals("tag") && children.get(1).getAttribute("type").equals("namex") &&
				children.size() == 4) {
				
				if (children.get(0).getValue().equals(children.get(2).getValue())) {
					e = new Entry(children.get(0).getValue(), children.get(1).getValue());
				}
				else {
					e = new Entry(children.get(0).getValue(), children.get(1).getValue(), children.get(2).getValue());
				}
				this.entryWords.add(e);
			}
			throw new Exception("data validation errors in children. breakpoint recommended");
		}
	}

}
