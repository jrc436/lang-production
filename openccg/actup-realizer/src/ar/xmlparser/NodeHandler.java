package ar.xmlparser;

import java.util.ArrayList;
import java.util.Stack;

//this keeps track of child relationships
public class NodeHandler {
	//the node on top will always be the most recent to add children to
	private Stack<XmlNode> activeNodes;
	private ArrayList<XmlNode> topLevelNodes; //where the data is actually stored at the end, as a list of trees
	
	public NodeHandler() {
		activeNodes = new Stack<XmlNode>();
		topLevelNodes = new ArrayList<XmlNode>();
	}
	
	//this is for an unmatched node that's not closed the same line
	public void AddOpenNode(XmlNode newNode) {
		AddClosedNode(newNode);
		activeNodes.push(newNode);
	}
	
	//this is for a standard line, where it's closed the same line as it's opened
	public void AddClosedNode(XmlNode newNode) {
		if (!activeNodes.isEmpty()) {
			activeNodes.peek().addChild(newNode);
		}
		else {
			topLevelNodes.add(newNode);
		}
	}
	
	//this is just to make sure that the read-in tag is the same as the top of the stack, otherwise the parser made a mistake
	public void CloseNode(String confirmTag, int lineNumber) throws Exception {
		XmlNode top = activeNodes.pop();
		if (!top.getLabel().equals(confirmTag)) {
			throw new Exception("labelOpen("+top.getLineNumber()+"):"+top.getLabel() + "; labelClosed("+lineNumber+"):"+confirmTag);
		}
	}
	public ArrayList<XmlNode> getTopLevelNodes() {
		return this.topLevelNodes;
	}
	
}
