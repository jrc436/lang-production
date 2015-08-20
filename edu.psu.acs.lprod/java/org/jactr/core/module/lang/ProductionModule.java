package org.jactr.core.module.lang;

import java.util.Collection;
import java.util.Comparator;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jactr.core.chunk.IChunk;
import org.jactr.core.chunk.lang.LangChunk;
import org.jactr.core.chunktype.IChunkType;
import org.jactr.core.model.IModel;
import org.jactr.core.module.declarative.IDeclarativeModule;
import org.jactr.core.module.declarative.associative.IAssociativeLinkageSystem;
import org.jactr.core.module.declarative.basic.AbstractDeclarativeModule;
import org.jactr.core.module.declarative.basic.DefaultDeclarativeModule;
import org.jactr.core.module.declarative.event.IDeclarativeModuleListener;
import org.jactr.core.module.declarative.five.IDeclarativeModule5;
import org.jactr.core.module.declarative.four.IDeclarativeModule4;
import org.jactr.core.module.declarative.four.learning.IDeclarativeLearningModule4;
import org.jactr.core.module.declarative.search.filter.IChunkFilter;
import org.jactr.core.module.declarative.six.DefaultDeclarativeModule6;
import org.jactr.core.production.request.ChunkTypeRequest;
import org.jactr.core.utils.parameter.IParameterized;
import org.jactr.core.utils.parameter.ParameterHandler;


public class ProductionModule extends DefaultDeclarativeModule6 implements IDeclarativeModule, IDeclarativeModule4, IDeclarativeModule5,
IParameterized {
	private static final Log LOGGER = LogFactory.getLog(DefaultDeclarativeModule.class);
	private static final int corpusYearsFactor = 250;
	public static final String SJI = "SJI_FACTOR";
	private int sjiFactor;
	public ProductionModule(String name) {
		super();
		// TODO Auto-generated constructor stub
		sjiFactor = corpusYearsFactor;
	}
	private int getSJIFactor() {
		return sjiFactor;
	}
	private void setSJIFactor(int factor) {
		this.sjiFactor = factor;
	}
	
	public void addDitransVerb(String name, String lex) {
		String uname = name.toUpperCase();
		
	}

	@Override
	protected IChunk addChunkInternal(IChunk chunk, Collection<IChunk> possibleMatches) {
		chunk.getSubsymbolicChunk().accessed(-473040000); //add a reference 15 years back
		chunk.getSubsymbolicChunk().accessed(-10022400);
		chunk.getSubsymbolicChunk().accessed(-5011200);
		chunk.getSubsymbolicChunk().setTimesInContext(30000); //from actup, if no presentation list given
		return super.addChunkInternal(chunk, possibleMatches); //not sure if this'll work yet.
	}
	 public void setParameter(String key, String value) {
		 if (SJI.equalsIgnoreCase(key)) {
			 setSJIFactor(ParameterHandler.numberInstance().coerce(value).intValue());
		 }
		 super.setParameter(key, value);
	 }
	 public String getParameter(String key) {
		 if (SJI.equalsIgnoreCase(key)) {
			 return getSJIFactor()+"";
		 }
		 return super.getParameter(key);
	 }
	 public Collection<String> getSetableParameters() {
		 Collection<String> rtn = super.getSetableParameters();
		 rtn.add(SJI);
		 return rtn;
	 }

}
