package ar.xmlparser;

import java.util.ArrayList;
import java.util.HashMap;

public class XmlNode {
	final private String label;
	final private String value;
	private ArrayList<XmlNode> children;
	private HashMap<String, String> attributes;
	
	//always use CreateXmlNode instead for parsing reasons
	public XmlNode(String label, String value) {
		this.label = label;
		this.value = value;
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
}
