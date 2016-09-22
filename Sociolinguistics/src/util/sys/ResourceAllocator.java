package util.sys;

import java.math.BigDecimal;
import java.math.RoundingMode;

public class ResourceAllocator {
	public static int getSuggestedNumThreads(int memGBPerThread) {
		 long systemMemB = Runtime.getRuntime().maxMemory();
         BigDecimal memReqGB = new BigDecimal(memGBPerThread);
         BigDecimal bytesPerGB = new BigDecimal(1000000000);
         BigDecimal memoryInGB = new BigDecimal(systemMemB);
         BigDecimal divided = memoryInGB.divide(bytesPerGB, 10, RoundingMode.HALF_UP).divide(memReqGB, 10, RoundingMode.HALF_UP);
         int memoryAllows = divided.intValue();
         int processors = Runtime.getRuntime().availableProcessors();
         return Math.max(1, Math.min(memoryAllows, processors));
	}
}
