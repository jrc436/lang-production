package edu.psu.acs.lang.output.eval.ngram;

import java.util.ArrayList;
import java.util.List;

public class TreebankTokenize {
	public static List<String> tokenize(String input) {
			  if (input.length() == 0) {
				  return new ArrayList<String>();
			  }
			  // Does the following things in order of appearance by line:
			  // 1. Replace quotes at the sentence start position with double ticks
			  // 2. Wrap spaces around a double quote preceded by opening brackets
			  // 3. Wrap spaces around a non-unicode ellipsis
			  // 4. Wrap spaces around some punctuation signs (,;@#$%&)
			  // 5. Wrap spaces around a period and zero or more closing brackets
			  //    (or quotes), when not preceded by a period and when followed
			  //    by the end of the string. Only splits final periods because
			  //    sentence tokenization is assumed as a preprocessing step
			  // 6. Wrap spaces around all exclamation marks and question marks
			  // 7. Wrap spaces around opening and closing brackets
			  // 8. Wrap spaces around en and em-dashes
//			  let parse = input.replace(/^\"/, ' `` ')
//			                   .replace(/([ (\[{<])"/g, '$1 `` ')
//			                   .replace(/\.\.\.*/g, ' ... ')
//			                   .replace(/[;@#$%&]/g, ' $& ')
//			                   .replace(/([^\.])(\.)([\]\)}>"\']*)\s*$/g, '$1 $2$3 ')
//			                   .replace(/[,?!]/g, ' $& ')
//			                   .replace(/[\]\[\(\)\{\}<>]/g, ' $& ')
//			                   .replace(/---*/g, ' -- ');

			  // Wrap spaces at the start and end of the sentence for consistency
			  // i.e. reduce the number of Regex matches required
//			  parse = ` ${parse} `;

			  // Does the following things in order of appearance by line:
			  // 1. Replace double quotes with a pair of single quotes wrapped with spaces
			  // 2. Wrap possessive or closing single quotes
			  // 3. Add a space before single quotes followed by `s`, `m`, or `d` and a space
			  // 4. Add a space before occurrences of `'ll`, `'re`, `'ve` or `n't`
//			  parse = parse.replace(/"/g, ' \'\' ')
//			               .replace(/([^'])' /g, '$1 \' ')
//			               .replace(/'([sSmMdD]) /g, ' \'$1 ')
//			               .replace(/('ll|'LL|'re|'RE|'ve|'VE|n't|N'T) /g, ' $1 ');
//
//			  let iterator = -1;
//			  while (iterator++ < TREEBANK_CONTRACTIONS.length) {
			    // Break uncommon contractions with a space and wrap-in spaces
//			    parse = parse.replace(TREEBANK_CONTRACTIONS[iterator], ' $1 $2 ');
//			  }

			  // Concatenate double spaces and remove start/end spaces
//			  parse = parse.replace(/\ \ +/g, ' ')
//			               .replace(/^\ |\ $/g, '');

			  // Split on spaces (original and inserted) to return the tokenized result
//			  return parse.split(' ');
//			}
			  throw new RuntimeException("Not implemented: placeholder");
	}
}
