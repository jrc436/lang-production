package edu.psu.acs.lang.gen;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.core.IModelElement;

public class ModelWriter {
	private final FileWriter fw;
	private final String modelName;
	private final ModelCreator mc;
	public ModelWriter(Path path, String modelName, ModelCreator mc) throws IOException {
		fw = new FileWriter(path.toFile());
		this.modelName = modelName;
		this.mc = mc;
	}
	public ModelWriter(Path path) throws IOException {
		fw = new FileWriter(path.toFile());
		modelName = null;
		mc = null;
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
	public void writeAll(Path typesList, Path wordInfo, String delimiter, Path sentFile) throws IOException {
		if (mc == null) {
			throw new IllegalArgumentException("need a modelcreator, this modelwriter has to manually write!");
		}
		List<IModelElement> writer = new ArrayList<IModelElement>();
		writer.addAll(getIntro(modelName, false));
		writer.add(new SimpleElement("<declarative-memory>"));
		writer.addAll(mc.makeChunkTypes());
		writeOut("Finished making chunk types", writer);
		writer.addAll(mc.makeConjChunks());
		writer.addAll(mc.makeOperatorChunks());
		writer.addAll(mc.makeTypeChunks(typesList));
		writer.addAll(mc.makeEmptyChunk());
		writeOut("Finished making type chunks", writer);
		writer.addAll(mc.makeLexSynAndWordChunks(wordInfo, delimiter));
		writeOut("Finished making lex syns", writer);
		
	//	writer.addAll(mc.makeSentenceManagers(allSentences));
		writer.addAll(mc.makeSentences(sentFile));
		writeOut("Finished making sentences", writer);
		
		writer.add(new SimpleElement("</declarative-memory>"));
		writer.add(new SimpleElement("<procedural-memory>"));
		writer.addAll(mc.makeRules());
		writer.add(new SimpleElement("</procedural-memory>")); 
		writeOut("Finished making rules", writer);	
		writer.addAll(getOutro());
		writeOut("Finished!", writer);
	}
//	public static void main(String[] args) throws IOException, URISyntaxException, ParseException {
//		int numSentences = ExperimentSettings.numberSentencesDesired;
//		String exp = ExperimentSettings.expName;
//		String sexpDir = FilePathConsts.expDir;
////		String swbdName = FilePathConsts.swbdBaseName;
////		String dataDirStr = FilePathConsts.dataDirName;
////		String projectName = FilePathConsts.projectName;
////		String modelsName = FilePathConsts.models;
//		int maxl = ExperimentSettings.maxLength;
//		
//		Path expDir = Paths.get(sexpDir);
//		expDir.toFile().mkdir();
//		
//		Path dataDir = FilePathConsts
//		
//		Random r = new Random();
//		Set<Integer> experimentDivisions = new HashSet<Integer>();
//		while (experimentDivisions.size() < 1) {
//			experimentDivisions.add(r.nextInt(2736));
//		}
//		int i = 0;
//		for (Integer divisionToUse : experimentDivisions) {
//			exp += i;
//		//	File workingDir = new File(ModelCreator.class.getProtectionDomain().getCodeSource().getLocation().toURI().getPath());
//		//	Path basePath = workingDir.getParentFile().getParentFile().toPath();
//		//	Path dataDir = basePath.resolve(dataDirStr);
////			
////			SWBDSplitter swsp = new SWBDSplitter(dataDir, swbdName);
////			
////			int numDivisions = swsp.split(numSentences, expDir, maxl);
////			ModelPreparer prep = new ModelPreparer(expDir, divisionToUse);
////			ModelCreator mc = new ModelCreator(prep, expDir);
////			Path modelDir = basePath.resolve(projectName).resolve(modelsName);
////			ModelWriter mw = new ModelWriter(modelDir.resolve("lp-"+exp+".jactr"));
//					
//		
//			i++;
//		}
//	}
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
