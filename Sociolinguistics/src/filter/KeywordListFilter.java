//package filter;
//
//import java.util.ArrayList;
//import java.util.Collection;
//import java.util.List;
//import java.util.Map.Entry;
//
//import util.data.Comment;
//import util.listdata.KeywordList;
//import util.sys.DataType;
//import util.wordmap.Combinable;
//import util.wordmap.WordMap;
//
//public abstract class KeywordListFilter extends Filter {
//	protected abstract IKeywordListFilter createFilter(KeywordList kl);
//	@Override
//	public void filter(DataType dt) {
//		KeywordList kl = (KeywordList) dt;
//		IKeywordListFilter filt = createFilter(kl);
//		
//	}
//	private static void applyFilter(KeywordList kl, IKeywordListFilter filt) {
//		List<String> mark = new ArrayList<String>();
//		for (Entry<String, Collection<Comment>> word : kl.entrySet()) {
//			if (!filt.keep(word.getKey())) {
//				mark.add(word.getKey());
//			}
//		}
//		for (String w : mark) {
//			wm.remove(w);
//		}
//	}
//
//}
