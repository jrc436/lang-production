package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

public class ProductionRule implements IProductionRule {

	private List<BufferConditions> conditions;
	private List<BufferEffects> effects;
	private String name;
	
	public ProductionRule(String name) {
		conditions = new ArrayList<BufferConditions>();
		effects = new ArrayList<BufferEffects>();
		this.name = name;
	}
	public ProductionRule(String name, BufferConditions bc1, BufferEffects be1) {
		this(name);
		conditions.add(bc1);
		effects.add(be1);
	}
	public ProductionRule(String name, BufferConditions bc1, BufferEffects be1, BufferConditions bc2, BufferEffects be2) {
		this(name, bc1, be1);
		conditions.add(bc2);
		effects.add(be2);
	}
	public ProductionRule(String name, BufferConditions bc1, BufferEffects be1, BufferConditions bc2) {
		this(name, bc1, be1);
		conditions.add(bc2);
	}
	public ProductionRule(String name, BufferConditions bc1, BufferEffects be1, BufferEffects be2) {
		this(name, bc1, be1);
		effects.add(be2);
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
		lines.add("</actions>");
		lines.add("</production>");
		return lines;
	}

}
