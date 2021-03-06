package edu.psu.acs.lang.declarative.chunk;

import java.util.List;

import edu.psu.acs.lang.core.IModelElement;
import edu.psu.acs.lang.declarative.slot.SlotValue;

/**
 * This is a system for generating chunks for a model. They are stored as java first, which then generates the XML for the model.
 * @author jrc
 *
 */
public interface IChunkStore extends IModelElement, SlotValue {
	/**
	 * Creates the XML for this chunk
	 * @return
	 * A list of Strings. If all of the chunks are combined, and added together, it serves as the declarative memory portion of the model.
	 */
	public abstract List<String> chunkTypeXML();
}
