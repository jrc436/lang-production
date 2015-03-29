///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2010 Michael White
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//////////////////////////////////////////////////////////////////////////////

package opennlp.ccg.parse;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import opennlp.ccg.grammar.Grammar;
import opennlp.ccg.hylo.HyloHelper;
import opennlp.ccg.hylo.Nominal;
import opennlp.ccg.lexicon.Tokenizer;
import opennlp.ccg.parse.supertagger.WordAndPOSDictionaryLabellingStrategy;
import opennlp.ccg.synsem.Category;
import opennlp.ccg.synsem.LF;
import opennlp.ccg.synsem.Sign;
import opennlp.ccg.synsem.SignScorer;
import opennlp.ccg.test.RegressionInfo;
import opennlp.ccgbank.extract.Testbed;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

/**
 * Creates a testbed file by parsing a text file.
 * Text is assumed to be tokenized, with one sentence per line.
 *
 * @author      Michael White
 * @version     $Revision: 1.2 $, $Date: 2010/10/28 02:46:32 $
 */

//parser config
//public static final int nBestListSize = 1;
//public static final String defaultScorer = "opennlp.ccg.ngrams.NgramPrecisionModel";
//public static final String defaultConfig = "../../config/tagger/stconfig";
public class ParseMain {		
	public void parseMain(String grammarfile, String inputfile, String outputfile, String parseScorerClass, String stconfig, int nBestListSize) throws IOException {
              
		// make test doc, sign map
		Document outDoc = new Document();
		Element outRoot = new Element("regression");
		outDoc.setRootElement(outRoot);
		Map<String,Sign> signMap = new HashMap<String,Sign>();

        // load grammar
        URL grammarURL = new File(grammarfile).toURI().toURL();
        System.out.println("Loading grammar from URL: " + grammarURL);
        Grammar grammar = new Grammar(grammarURL);
        Tokenizer tokenizer = grammar.lexicon.tokenizer;
        System.out.println();
        
        // set up parser
        Parser parser = new Parser(grammar);
        // instantiate scorer
        try {
            System.out.println("Instantiating parsing sign scorer from class: " + parseScorerClass);
            SignScorer parseScorer = (SignScorer) Class.forName(parseScorerClass).newInstance();
            parser.setSignScorer(parseScorer);
            System.out.println();
        } 
        catch (Exception exc) {
            throw (RuntimeException) new RuntimeException().initCause(exc);
        }
        // instantiate supertagger
        try {
        	Supertagger supertagger;
    		System.out.println("Instantiating supertagger from config file: " + stconfig);
    		supertagger = WordAndPOSDictionaryLabellingStrategy.supertaggerFactory(stconfig);
            parser.setSupertagger(supertagger);
            System.out.println();
        } 
        catch (Exception exc) {
            throw (RuntimeException) new RuntimeException().initCause(exc);
        }
        
        // loop through input
        BufferedReader in = new BufferedReader(new FileReader(inputfile));
        String line;
        Map<String,String> predInfoMap = new HashMap<String,String>();
        System.out.println("Parsing " + inputfile);
        System.out.println();
        int count = 1;
        while ((line = in.readLine()) != null) {
        	String id = "s" + count;
        	try {
        		// parse it
        		System.out.println(line);
				parser.parse(line);
				int numParses = Math.min(nBestListSize, parser.getResult().size());
				for (int i=0; i < numParses; i++) {
				    Sign thisParse = parser.getResult().get(i);
				    // convert lf
				    Category cat = thisParse.getCategory();
				    LF convertedLF = null;
				    String predInfo = null;
				    if (cat.getLF() != null) {
					// convert LF
					LF flatLF = cat.getLF();
					cat = cat.copy();
					Nominal index = cat.getIndexNominal(); 
					convertedLF = HyloHelper.compactAndConvertNominals(flatLF, index, thisParse);
					// get pred info
					predInfoMap.clear();
					Testbed.extractPredInfo(flatLF, predInfoMap);
					predInfo = Testbed.getPredInfo(predInfoMap);
				    }
				    // add test item, sign
				    Element item = RegressionInfo.makeTestItem(grammar, line, 1, convertedLF);
				    String actualID = (nBestListSize == 1) ? id : id + "-" + (i+1);
				    item.setAttribute("info", actualID);
				    outRoot.addContent(item);
				    signMap.put(actualID, thisParse);
				    // Add parsed words as a separate LF element
				    Element fullWordsElt = new Element("full-words");
				    fullWordsElt.addContent(tokenizer.format(thisParse.getWords()));
				    item.addContent(fullWordsElt);
				    if (predInfo != null) {
						Element predInfoElt = new Element("pred-info");
						predInfoElt.setAttribute("data", predInfo);
						item.addContent(predInfoElt);
				    }
				}
			} 
        	catch (ParseException e) {
			    System.out.println("Unable to parse!");
			    // add test item with zero parses
			    Element item = RegressionInfo.makeTestItem(grammar, line, 0, null);
			    item.setAttribute("info", id);
			    outRoot.addContent(item);
        	}
        	count++;
        }
        System.out.println();
        
		// write test doc, saved signs
        System.out.println("Writing parses to " + outputfile);
		XMLOutputter outputter = new XMLOutputter(Format.getPrettyFormat());
		File regressionFile = new File(outputfile);
		outputter.output(outDoc, new FileOutputStream(regressionFile));
		RegressionInfo.writeSerFile(signMap, regressionFile);
        System.out.println();
		
        // done
        in.close();
        System.out.println("Done.");
	}
}
