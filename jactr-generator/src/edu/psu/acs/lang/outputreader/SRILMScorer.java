package edu.psu.acs.lang.outputreader;

import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class SRILMScorer implements Evaluator {
	private final String commandPath;
	private final String lmPath;
//	private static final String tmpOutput = "output.txt";
	private static final String tmpInput = "input.txt";
	private Path tempDir;
	public SRILMScorer(String commandPath, String lmPath) throws IOException {
		this(commandPath, lmPath, 0);
	}
	public SRILMScorer(String commandPath, String lmPath, int threadnum) throws IOException {
		tempDir = Files.createTempDirectory("score"+threadnum);
		this.commandPath = commandPath;
		this.lmPath = lmPath;
	}
	private String getInputPath() {
		return tempDir.resolve(tmpInput).toString();
	}
//	private String getOutputPath() {
//		return tempDir.resolve(tmpOutput).toString();
//	}
	private String getCommand() {
		return commandPath+" -lm "+lmPath+" -unk -ppl "+getInputPath();//+" > "+getOutputPath();
	}
	private double parseLogProb(List<String> outputFileLines) {
		//this is just hardcoded from my experience with SRILM. Not intended to be robust
		String line = outputFileLines.get(1);
		String[] parts = line.split(" ");
		return Double.parseDouble(parts[3]);
	}
	@Override
	public double score(String s) {
		Runtime rt = Runtime.getRuntime();	
		try {
			FileWriter fw = new FileWriter(getInputPath());
			fw.write(s);
			fw.close();
			Process p = rt.exec(getCommand());
			int error = p.waitFor();
			if (error == 0) {
				List<String> lines = new ArrayList<String>();
				InputStream in = p.getInputStream();
				Scanner scan = new Scanner(in);
				while (scan.hasNext()) {
					lines.add(scan.nextLine());
				}
				scan.close();
				in.close();
				return Evaluator.convertToProb(parseLogProb(lines));
			}
			else {
				System.err.println("Process exited with nonzero error status");
				
				InputStream err = p.getErrorStream();
				Scanner scan = new Scanner(err);
				while (scan.hasNext()) {
					System.err.println(scan.nextLine());
				}
				scan.close();
				err.close();
				System.exit(1);
			}
		}
		catch (InterruptedException ue) {
			ue.printStackTrace();
			System.exit(1);
		}
		catch (IOException ie) {
			ie.printStackTrace();
			System.exit(1);
		}
		return 0.0;
	}
}
