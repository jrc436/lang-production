package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;

public class ProductionRule implements IProductionRule {
	public static final String RetrievalBufferVar = "retrieval";
	private List<BufferConditions> conditions;
	private List<BufferEffects> effects;
	private List<String> outputs;
	private String name;
	
	protected void addOutput(String s) {
		outputs.add(s);
	}
	protected void requireGoal(List<ISlot> slots) {
		conditions.add(new BufferConditions(Buffer.goal, ChunkTypeEnum.sentence, slots));
	}
	protected void requireRetrieval(List<ISlot> slots, ChunkTypeEnum type) {
		conditions.add(new BufferConditions(Buffer.retrieval, type, slots));
	}
	protected void makeRetrieval(ChunkTypeEnum chunk, List<ISlot> vars) {
		effects.add(new BufferEffects(chunk, vars, Buffer.retrieval));
	}
	protected void modifyGoal(List<ISlot> vars) {
		effects.add(new BufferEffects(vars, Buffer.goal));
	}
	protected void makeQuery(BufferQueries query) {
		conditions.add(query);
	}
	
	//remove the lexsyn from the goal buffer
	public static List<Slot> wipeOut(int cueNum, int maxwords, int maxTypes) {
		List<Slot> effects = new ArrayList<Slot>();
		effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynString, cueNum, maxwords), new NullValue()));
		for (int i = 1; i <= maxTypes; i++) {
			effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, cueNum, maxwords, i, maxTypes), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, cueNum, maxwords, i, maxTypes), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, cueNum, maxwords, i, maxTypes), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, cueNum, maxwords, i, maxTypes), new NullValue()));
		}
		return effects;
	}
	//remove the unused types of a combined lexsyn from the goal buffer
	public static List<Slot> cleanNonCombinedTypes(int lexsynNum, int chosenType, int maxwords, int maxTypes) {
		List<Slot> effects = new ArrayList<Slot>();
		for (int i = 1; i <= maxTypes; i++) {
			if (i == chosenType) {
				continue;
			}
			effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynNum, maxwords, i, maxTypes), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynNum, maxwords, i, maxTypes), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynNum, maxwords, i, maxTypes), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynNum, maxwords, i, maxTypes), new NullValue()));
		}
		return effects;
	}
	
	public ProductionRule(String name) {
		conditions = new ArrayList<BufferConditions>();
		effects = new ArrayList<BufferEffects>();
		this.name = name;
		this.outputs = new ArrayList<String>();
	}
	@Override
	public List<String> toXML() {
		List<String> lines = new ArrayList<String>();
		lines.add("<production name=\""+name+"\">");
		lines.add("<conditions>");
		for (BufferConditions c : conditions) {
			lines.addAll(c.toXML());
		}
		lines.add("</conditions>");
		lines.add("<actions>");
		for (BufferEffects effect : effects) {
			lines.addAll(effect.toXML());
		}
		for (String out : outputs) {
			lines.add("<output>\""+out+"\"</output>");
		}
		lines.add("</actions>");
		//can add parameters here to change the way production timing, utility, etc. works
		lines.add("</production>");
		return lines;
	}

}
