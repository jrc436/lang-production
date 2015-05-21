///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2004-5 University of Edinburgh (Michael White)
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

package ngrams;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.util.ArrayList;
import java.util.List;

import lexicon.IWordFactory;
import lexicon.Tokenizer;
import util.TrieMap;

/**
 * A scorer for a standard n-gram backoff model. 
 * Unknown words are mapped to &lt;unk&gt; if the latter is present in 
 * the model.
 *
 * @author      Michael White
 * @version     $Revision: 1.19 $, $Date: 2011/10/11 03:29:42 $
 */
public class StandardNgramModel extends AbstractStandardNgramModel
{
	public StandardNgramModel(int order, String filename, IWordFactory wf, Tokenizer t) throws IOException {
		this(order, filename, false, new double[0], wf, t);
	 }
	 public StandardNgramModel(int order, String filename, double[] varValues, IWordFactory wf, Tokenizer t) throws IOException {
		 this(order, filename, false, varValues, wf, t);
	 }
	
	/** 
     * Loads an n-gram model of the given order in ARPA (Doug Paul) format from
     * the given reader, with the given flag controlling whether words are
     * replaced by their semantic classes.
	 * @param wf TODO
	 * @param t TODO
     */
    public StandardNgramModel(int order, String filename, boolean useSemClasses, double[] varValues, IWordFactory wf, Tokenizer t)
    		throws IOException {
        super(order, useSemClasses, wf, t, varValues);
        openVocab = readModel(new BufferedReader(new FileReader(filename)));
    }
	
	// reads in model
	//return whether or not the file was open
    private boolean readModel(Reader in) throws IOException {
        StreamTokenizer tokenizer = initTokenizer(in); 
        String[] tokens = new String[order+2];
        boolean foundData = false;
        int currentOrder = 0;
        List<String> currentPrefix = new ArrayList<String>();
        List<String> currentKeys = null;
        List<TrieMap<String,NgramFloats>> currentChildren = null;
        // loop through lines
        while (tokenizer.ttype != StreamTokenizer.TT_EOF) {
            // read line into tokens
            readLine(tokenizer, tokens);
            // check for blank line
            if (tokens[0] == null) continue;
            // check for initial delimiter
            if (tokens[0].equals("\\data\\")) { foundData = true; continue; }
            if (!foundData) continue;
            // read header line
            if (tokens[0].equals("ngram")) {
                int n = Integer.parseInt(tokens[1].substring(0,1)); 
                int total = Integer.parseInt(tokens[1].substring(2));
                if (n > order) continue;
                numNgrams[n-1] = total;
                // init children, keys lists
                if (currentChildren == null) { 
                    currentChildren = new ArrayList<TrieMap<String,NgramFloats>>(total);  
                    currentKeys = new ArrayList<String>(total);  
                }
                // calc totals (not actually used anymore)
                if (n == order) {
                    @SuppressWarnings("unused")
					int totalNgrams = 0;
                    for (int i = 0; i < order; i++) { totalNgrams += numNgrams[i]; }
                    // System.out.println("totalNgrams: " + totalNgrams);
                }
                continue;
            }
            // check for final delimiter
            if (tokens[0].equals("\\end\\")) {
                // add current children
                addTrieMapChildren(currentPrefix, currentKeys, currentChildren);
                break;
            }
            // read line starting new order
            if (tokens[0].equals("\\" + (currentOrder+1) + "-grams:")) { 
                // add current children
                addTrieMapChildren(currentPrefix, currentKeys, currentChildren);
                // System.out.println(tokens[0]);
                currentOrder++; continue; 
            } 
            if (currentOrder == 0) continue;
            if (currentOrder > order) break;
            // read logprob
            float logprob = Float.parseFloat(tokens[0]);
            // read back-off weight (except with last order)
            float bow = 0;
            if (currentOrder < order && tokens[currentOrder+1] != null) {
                bow = Float.parseFloat(tokens[currentOrder+1]);
            }
            // intern string tokens
            for (int i = 1; i < currentOrder+1; i++) {
                tokens[i] = tokens[i].intern();
            }
            // check prefix
            boolean samePrefix = (currentPrefix.size() == currentOrder-1);
            for (int i = 1; samePrefix && i < currentOrder; i++) {
                if (tokens[i] != currentPrefix.get(i-1)) samePrefix = false; 
            }
            // if changed, add current children, reset prefix 
            if (!samePrefix) {
                addTrieMapChildren(currentPrefix, currentKeys, currentChildren);
                for (int i = 1; i < currentOrder; i++) {
                    currentPrefix.add(tokens[i]);
                }
            }
            String key = tokens[currentOrder];
            currentKeys.add(key);
            currentChildren.add(new TrieMap<String,NgramFloats>(new NgramFloats(logprob, bow)));
        }
        // set openVocab according to presence of <unk>
        return (trieMapRoot.getChild("<unk>") != null);
    }
}
