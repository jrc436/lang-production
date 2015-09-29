package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;

public class ProductionRule implements IProductionRule {

	protected List<BufferConditions> conditions;
	protected List<BufferEffects> effects;
	protected List<String> outputs;
	private String name;
	
	public static List<Slot> wipeOut(int cueNum, int maxTypes) {
		List<Slot> effects = new ArrayList<Slot>();
		effects.add(new Slot(new SSlotName(SSlotNameEnum.Cue, cueNum), new NullValue()));
		for (int i = 1; i <= maxTypes; i++) {
			effects.add(new Slot(new SSlotName(SSlotNameEnum.CueType, cueNum, i), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.CueLeftType, cueNum, i), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.CueRightType, cueNum, i), new NullValue()));
			effects.add(new Slot(new SSlotName(SSlotNameEnum.CueCombo, cueNum, i), new NullValue()));
		}
		return effects;
	}
	
	public ProductionRule(String name, List<BufferConditions> cond, List<BufferEffects> eff, List<String> outputs) {
		conditions = cond;
		effects = eff;
		this.name = name;
		this.outputs = outputs;
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
			lines.add("<output>"+out+"</output>");
		}
		lines.add("</actions>");
		lines.add("</production>");
		return lines;
	}

}
