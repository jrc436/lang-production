package ar.xmlparser;

import java.util.ArrayList;
import java.util.HashMap;

public class XmlNode {
	final private String label;
	final private String value;
	final private int lineNumber; //mostly just for debugging purposes
	private ArrayList<XmlNode> children;
	private HashMap<String, String> attributes;
	
	//always use CreateXmlNode instead for parsing reasons
	public XmlNode(String label, String value, int lineNumber) {
		this.label = label;
		this.value = value;
		attributes = new HashMap<String, String>();
		children = new ArrayList<XmlNode>();
		if (lineNumber == 0) {
			System.err.println("what the fawk");
		}
		this.lineNumber = lineNumber;
	}
	public void addAttribute(String attrName, String attrValue) {
		this.attributes.put(attrName, attrValue);
	}
	public void addChild(XmlNode child) {
		this.children.add(child);
	}
	public String getLabel() {
		return label;
	}
	public String getValue() {
		return value;
	}
	public int getLineNumber() {
		return lineNumber;
	}
	public String getAttribute(String name) {
		return attributes.get(name);
	}
	public ArrayList<XmlNode> getChildren() {
		return children;
	}
}
