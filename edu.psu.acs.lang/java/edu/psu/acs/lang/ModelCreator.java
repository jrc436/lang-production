package edu.psu.acs.lang;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class ModelCreator {
	List<IModelElement> elements;
	private ModelCreator() {
		elements = new ArrayList<IModelElement>();
	}
	private void addChunks(List<IModelElement> elsSoFar) {
		
	}
	
	public static void main(String[] args) throws IOException {
		ModelCreator mc = new ModelCreator();
		FileWriter write = new FileWriter("../../../../../models/genlangprod.jactr");
		for (IModelElement el : mc.elements) {
			List<String> lines = el.toXML();
			for (String l : lines) {
				write.write(l+"\n");
			}
		}
		write.close();
	}
}
