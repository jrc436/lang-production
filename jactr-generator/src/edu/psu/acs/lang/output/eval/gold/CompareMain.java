package edu.psu.acs.lang.output.eval.gold;
//package edu.psu.acs.lang.eval.target;
//
//import java.io.File;
//import java.io.IOException;
//import java.nio.file.Paths;
//
//public class CompareMain {
//	public static void main(String[] args) throws IOException {
//		Rouge r = new Rouge();
//		File inpDir = Paths.get(args[0]).toFile();
//		File goldDir = Paths.get(args[1]).toFile();
//		System.out.println(Comparer.evaluateAllAvg(r::n, inpDir, goldDir));
//		System.out.println(Comparer.evaluateAllAvg(r::l, inpDir, goldDir));
//		System.out.println(Comparer.evaluateAllAvg(r::s, inpDir, goldDir));
//	}
//}
