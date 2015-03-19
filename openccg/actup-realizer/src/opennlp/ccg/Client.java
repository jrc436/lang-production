package opennlp.ccg;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Paths;

//forgot to commit a simple file and don't want to cause git collisions

public class Client {
	private static final String basePath = "/Users/jrc/Public/jrc-research/";
	//private static final String basePath = "/work/research/";
	private static final String trialName = "full-eval";
	
	private static final String experimentPath = basePath+"ap-largefiles/experiment/";
	private static final String outPath = experimentPath+"output/"+trialName+".txt";
	private static final String grammar = basePath+"actup-production/openccg/config/grammar/grammar.xml";	
	private static final String wsj_lm_dir = experimentPath+"wsj-lm/";
	private static final String swbd_lm_dir = experimentPath+"swbd-lm/";
	private static final String trial_dir = experimentPath+"actr-trial/";
	private static final String rouge_path = basePath+"actup-production/rouge/";
	
	//parser config
//	public static final int nBestListSize = 1;
//	public static final String defaultScorer = "opennlp.ccg.ngrams.NgramPrecisionModel";
//	public static final String defaultConfig = "../../config/tagger/stconfig";
	public static final String parsedPath = experimentPath+"swbd-parsed/";

	
	
	//realizer config
	private static final String trialPath = trial_dir+trialName+"/";
	private static final boolean ACTRTrial = true;	
	
	public static void main(String[] args) throws Exception {
		Realize r = new Realize();
		r.Initialize(grammar);
		//Parse p = new Parse();
		
		String runType = ACTRTrial ? "actr-" : "std-";
		
		File[] input = new File(parsedPath).listFiles();
		int fileCount = input.length; //this is just to make the config at the end.
		
		try {
			Files.createDirectory(Paths.get(trialPath));
		}
		catch (Exception e) {
			System.err.print("YOU ARE OVERWRITING A RUN!!!!!");
		}
		String type = "";
//		//WSJ time
		r.setLM(ACTRTrial, wsj_lm_dir+"wsj.lm");
		type = runType + "wsj-";
		for (File in : input) {
			String num = in.getName().split("-")[1];
			System.out.println("Beginning to realize: "+num);
			System.out.println("This realization is a: " + type);
			String outPath = trialPath+type+num+".spl";
			r.realize(in.getPath(), outPath);
		}
		
		//SWM1 time
		type = runType + "swm1-";
		for (File in : input) {
			String num = in.getName().split("-")[1];
			System.out.println("Beginning to realize: "+num);
			System.out.println("This realization is a: " + type);
			r.setLM(ACTRTrial, swbd_lm_dir+"sw-"+num+".lm");
			String outPath = trialPath+type+num+".spl";
			r.realize(in.getPath(), outPath);
		}
		
		//evaluation time
		
		//First, copy the output to the rouge directory
		//cp trial_dir/* rouge_path/realizations/
		//then run the rouge evaluation and pipe it to the output directory
		Runtime run = Runtime.getRuntime();
		execute(run, "cp "+trialPath+"/* "+rouge_path+"realizations/");	
		execute(run, "python "+rouge_path+"make-config.py "+fileCount+" "+ rouge_path);
		execute(run, "perl "+rouge_path+"rouge.pl -a "+rouge_path+"config.xml > "+outPath);
	}
	private static void execute(Runtime r, String com) throws IOException {
		Process p = r.exec(new String[]{"/bin/sh", "-c", com});
		String s;
		BufferedReader stdin = new BufferedReader(new InputStreamReader(p.getInputStream()));
		BufferedReader stderr = new BufferedReader(new InputStreamReader(p.getErrorStream()));
		
		s = null;
		while ((s = stderr.readLine()) != null) {
			System.out.println("Error from: "+com);
			System.out.println(s);
		}
		
		s = null;
		while ((s = stdin.readLine()) != null) {
			System.out.println("Output from: "+com);
			System.out.println(s);
		}
	}
}
