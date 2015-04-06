package evaluation;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import runconfig.ScoringStrategy;
import runconfig.ShellScriptExecute;

public class Rouge extends Evaluator {

	private String rougePath;
	ShellScriptExecute exc;
	public Rouge(ScoringStrategy strat, String rougePath) {
		super(strat);
		this.rougePath = rougePath;
		exc = new ShellScriptExecute(Runtime.getRuntime());
	}
	
	@Override
	public double score(String eval, String gold) {	
		double toReturn = -1.0;
		try {
			Path e = Files.createTempFile("eval", null);
			Path g = Files.createTempFile("gold", null);
			Path conf = Files.createTempFile("config", "xml");
			Path out = Files.createTempFile("output", null);
			FileWriter fw = new FileWriter(e.toFile());
			fw.write(eval);
			fw.close();
			fw = new FileWriter(g.toFile());
			fw.write(gold);
			fw.close();
			fw = new FileWriter(conf.toFile());
			for (String s : getRougeList(e, g)) {
				fw.write(s);
			}
			fw.close();
			
			exc.execute("perl "+rougePath+" -a "+conf.toAbsolutePath().toString() +" >> "+out.toAbsolutePath().toString());
			List<String> lines = Files.readAllLines(out);
			for (String line : lines) {
				//just parse that sucker out
				if (line.contains("Average_F")) {
					toReturn = Double.parseDouble(line.substring(line.indexOf(":")+2, line.indexOf("(")));
				}
			}
		}
		catch (IOException e){
			e.printStackTrace();
		}
		return (1.0 - toReturn);
		
	}
	
	private ArrayList<String> getRougeList(Path ePath, Path gPath) {
		ArrayList<String> v = new ArrayList<String>();
		v.add("<ROUGE-EVAL version=\"1.0\">\n");
		v.add("<EVAL ID=\"tmp\"+>\n");
	    v.add("<PEER-ROOT>\n");
	    v.add(ePath.getParent().toAbsolutePath().toString()+"\n");
	    v.add("</PEER-ROOT>\n");
	    v.add("<MODEL-ROOT>\n");
	    v.add(gPath.getParent().toAbsolutePath().toString()+"\n");
	    v.add("</MODEL-ROOT>\n");
	    v.add("<INPUT-FORMAT TYPE=\"SPL\">\n");
	    v.add("</INPUT-FORMAT>\n");
	    v.add("<PEERS>\n");
	    v.add(ePath.toAbsolutePath().toString()+"\n");
	    v.add("</PEERS>\n");
        v.add("<MODELS>\n");
        v.add(gPath.toAbsolutePath().toString()+"\n");
	    v.add("</MODELS>\n");
        v.add("</EVAL>\n");
        v.add("</ROUGE-EVAL>");
		return v;
	}
	
}
