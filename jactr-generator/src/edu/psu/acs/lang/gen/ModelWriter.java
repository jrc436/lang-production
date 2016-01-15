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
import edu.psu.acs.lang.PathConsts;

public class ModelWriter {
	private static final int numDivisionsToUse = 1;
	private static final int numDivisions = 10000000;
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
	public static void main(String[] args) throws IOException, URISyntaxException {
		int divisions = numDivisions;
		int divisionsUse = numDivisionsToUse;
		String exp = PathConsts.expName;
		String swbdName = PathConsts.swbdBaseName;
		String dataDirStr = PathConsts.dataDirName;
		
		Random r = new Random();
		File workingDir = new File(ModelCreator.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());
		Path basePath = workingDir.getParentFile().getParentFile().toPath();
		Path dataDir = basePath.resolve(dataDirStr);
		Path expDir = dataDir.resolve(exp);
		SWBDSplitter swsp = new SWBDSplitter(dataDir, swbdName);
		divisions = swsp.split(divisions, expDir);
		int[] divisionsToUse = new int[divisionsUse];
		for (int i = 0; i < divisionsUse; i++) {
			divisionsToUse[i] = r.nextInt(divisions);
		}
		ModelPreparer prep = new ModelPreparer(expDir, divisionsToUse);
		ModelCreator mc = new ModelCreator(prep, expDir);
		Path modelDir = basePath.resolve("edu.psu.acs.lang/models/");
		ModelWriter mw = new ModelWriter(modelDir.resolve("lang_"+exp+".jactr"));
				
		List<IModelElement> writer = new ArrayList<IModelElement>();
		writer.addAll(getIntro(exp));
		writer.add(new SimpleElement("<declarative-memory>"));
		writer.addAll(mc.makeChunkTypes());
		mw.writeOut("Finished making chunk types", writer);
		writer.addAll(mc.makeOperatorChunks());
		writer.addAll(mc.makeTypeChunks());
		writer.addAll(mc.makeEmptyChunk());
		mw.writeOut("Finished making type chunks", writer);
		writer.addAll(mc.makeLexSynAndWordChunks());
		mw.writeOut("Finished making lex syns", writer);
		for (int division : divisionsToUse) {
			writer.addAll(mc.makeSentences(division));
			mw.writeOut("Finished making sentence: "+division, writer);
		}
		writer.add(new SimpleElement("</declarative-memory>"));
		writer.add(new SimpleElement("<procedural-memory>"));
		writer.addAll(mc.makeRules());
		writer.add(new SimpleElement("</procedural-memory>")); 
		mw.writeOut("Finished making rules", writer);	
		writer.addAll(getOutro());
		mw.writeOut("Finished!", writer);
	}
	private static List<SimpleElement> getIntro(String exp) {
		List<SimpleElement> intro = new ArrayList<SimpleElement>();
		intro.add(new SimpleElement("<actr>"));
		intro.add(new SimpleElement("<model name=\""+exp+"\">"));
		intro.add(new SimpleElement("<modules>"));
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.declarative.six.DefaultDeclarativeModule6\"/>"));
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.procedural.sixtrace.TracerProceduralModule6\"/>"));
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.goal.six.DefaultGoalModule6\"/>"));
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.procedural.six.learning.DefaultProceduralLearningModule6\"/>"));
		intro.add(new SimpleElement("<module class=\"org.jactr.core.module.retrieval.six.DefaultRetrievalModule6\">"));
		intro.add(new SimpleElement("<parameters>"));
		intro.add(new SimpleElement("<parameter name=\"LatencyFactor\" value=\"0.00\"/>"));
		intro.add(new SimpleElement("</parameters>"));
		intro.add(new SimpleElement("</module>"));
		intro.add(new SimpleElement("</modules>"));
		return intro;
	}
	private static List<SimpleElement> getOutro() {
		List<SimpleElement> intro = new ArrayList<SimpleElement>();
		intro.add(new SimpleElement("<buffer name=\"goal\" chunk=\"goal1\"/>"));
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
