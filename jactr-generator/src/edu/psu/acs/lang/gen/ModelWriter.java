package edu.psu.acs.lang.gen;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import edu.psu.acs.lang.IModelElement;
import edu.psu.acs.lang.util.ParseException;
import edu.psu.acs.lang.util.PathConsts;

public class ModelWriter {
	private static final int numberSentencesDesired = 100;
	private static final int maxLength = 10;
	private final FileWriter fw;
	public ModelWriter(Path path) throws IOException {
		fw = new FileWriter(path.toFile());
	}
	public void writeOut(String stdoutMessage, List<IModelElement> elements) throws IOException {
		for (IModelElement ime : elements) {
			for (String s : ime.toXML()) {
				fw.write(s+System.getProperty("line.separator"));
			}
		}
		System.out.println(stdoutMessage);
		fw.flush();
		elements.clear();
	}
	public static void main(String[] args) throws IOException, URISyntaxException, ParseException {
		int numSentences = numberSentencesDesired;
		String exp = PathConsts.expName;
		String swbdName = PathConsts.swbdBaseName;
		String dataDirStr = PathConsts.dataDirName;
		String projectName = PathConsts.projectName;
		String modelsName = PathConsts.models;
		int maxl = maxLength;
		
		Random r = new Random();
		File workingDir = new File(ModelCreator.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());
		Path basePath = workingDir.getParentFile().getParentFile().toPath();
		Path dataDir = basePath.resolve(dataDirStr);
		Path expDir = dataDir.resolve(exp);
		SWBDSplitter swsp = new SWBDSplitter(dataDir, swbdName);
		int numDivisions = swsp.split(numSentences, expDir, maxl);
		int divisionToUse = r.nextInt(numDivisions);
		ModelPreparer prep = new ModelPreparer(expDir, divisionToUse);
		ModelCreator mc = new ModelCreator(prep, expDir);
		Path modelDir = basePath.resolve(projectName).resolve(modelsName);
		ModelWriter mw = new ModelWriter(modelDir.resolve("lp-"+exp+".jactr"));
				
		List<IModelElement> writer = new ArrayList<IModelElement>();
		writer.addAll(getIntro(exp, false));
		writer.add(new SimpleElement("<declarative-memory>"));
		writer.addAll(mc.makeChunkTypes());
		mw.writeOut("Finished making chunk types", writer);
		writer.addAll(mc.makeConjChunks());
		writer.addAll(mc.makeOperatorChunks());
		writer.addAll(mc.makeTypeChunks());
		writer.addAll(mc.makeEmptyChunk());
		mw.writeOut("Finished making type chunks", writer);
		writer.addAll(mc.makeLexSynAndWordChunks());
		mw.writeOut("Finished making lex syns", writer);
		
	//	writer.addAll(mc.makeSentenceManagers(allSentences));
		writer.addAll(mc.makeSentences(divisionToUse));
		mw.writeOut("Finished making sentences in file: "+divisionToUse, writer);
		
		writer.add(new SimpleElement("</declarative-memory>"));
		writer.add(new SimpleElement("<procedural-memory>"));
		writer.addAll(mc.makeRules());
		writer.add(new SimpleElement("</procedural-memory>")); 
		mw.writeOut("Finished making rules", writer);	
		writer.addAll(getOutro());
		mw.writeOut("Finished!", writer);
	}
	private static List<SimpleElement> getIntro(String exp, boolean tracer) {
		List<SimpleElement> intro = new ArrayList<SimpleElement>();
		intro.add(new SimpleElement("<actr>"));
		intro.add(new SimpleElement("<model name=\""+exp+"\">"));
		intro.add(new SimpleElement("<modules>"));
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.declarative.six.DefaultDeclarativeModule6\"/>"));
		
		if (tracer) {
			intro.add(new SimpleElement("<module class=\"org.jactr.core.module.procedural.sixtrace.TracerProceduralModule6\"/>"));
		}
		else {
			intro.add(new SimpleElement("<module class=\"org.jactr.core.module.procedural.six.DefaultProceduralModule6\"/>"));
		}
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.goal.six.DefaultGoalModule6\"/>"));
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.procedural.six.learning.DefaultProceduralLearningModule6\"/>"));
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.retrieval.six.DefaultRetrievalModule6\">"));
		intro.add(new SimpleElement("<parameters>"));
		intro.add(new SimpleElement("<parameter name=\"LatencyFactor\" value=\"0.00\"/>"));
		intro.add(new SimpleElement("<parameter name=\"FINSTDurationTime\" value=\"30000.0\"/>"));
	    intro.add(new SimpleElement("<parameter name=\"NumberOfFINSTs\" value=\"1000\"/>"));
		intro.add(new SimpleElement("</parameters>"));
		intro.add(new SimpleElement("</module>"));
		intro.add(new SimpleElement("</modules>"));
		return intro;
	}
	private static List<SimpleElement> getOutro() {
		List<SimpleElement> intro = new ArrayList<SimpleElement>();
		intro.add(new SimpleElement("<buffer name=\"goal\"/>"));// chunk=\""+Sentence.getNameConst(1)+"\"/>"));
		intro.add(new SimpleElement("<buffer name=\"retrieval\">"));
		intro.add(new SimpleElement("<parameters>"));
		intro.add(new SimpleElement("<parameter name=\"StrictHarvestingEnabled\" value=\"true\"/>"));
		intro.add(new SimpleElement("</parameters>"));
		intro.add(new SimpleElement("</buffer>"));
		intro.add(new SimpleElement("<parameters>"));
		intro.add(new SimpleElement("<parameter name=\"EnableUnusedCycleSkipping\" value=\"true\"/>"));
		intro.add(new SimpleElement("<parameter name=\"EnablePersistentExecution\" value=\"false\"/>"));
		intro.add(new SimpleElement("</parameters>"));
		intro.add(new SimpleElement("</model>"));
		intro.add(new SimpleElement("</actr>"));
		return intro;
	}

}
