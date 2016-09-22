package filter;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Map.Entry;

import util.wordmap.Combinable;
import util.wordmap.EarlyDateCombine;
import util.wordmap.WordMap;

public class DateFilter extends WordMapFilter {
	private static final Instant earliestDate = LocalDate.of(2013, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant();
	private static final Instant latestDate = LocalDate.of(2014, 1, 1).atStartOfDay(ZoneId.systemDefault()).toInstant();
	@Override
	protected IWordMapFilter createFilter(WordMap wm) {
		return new IWordMapFilter() {
			@Override
			public boolean goodEntry(Entry<String, Combinable> entry) {
				Instant postTime =  ((EarlyDateCombine) wm.getBy(entry.getKey(), EarlyDateCombine.class)).getTime();
				return postTime.isAfter(earliestDate) && postTime.isBefore(latestDate);
			}
		};
	}
	
}
