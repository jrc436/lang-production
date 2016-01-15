package edu.psu.acs.lang.production;

import java.util.ArrayList;
import java.util.List;

import edu.psu.acs.lang.declarative.CCGTypeSlot;
import edu.psu.acs.lang.declarative.ChunkTypeEnum;
import edu.psu.acs.lang.declarative.ISlot;
import edu.psu.acs.lang.declarative.NullValue;
import edu.psu.acs.lang.declarative.SSlotName;
import edu.psu.acs.lang.declarative.SSlotNameEnum;
import edu.psu.acs.lang.declarative.Slot;
import edu.psu.acs.lang.declarative.SlotVar;

public class ResolveForwardApplication extends SyntaxRuleResolution {

	public ResolveForwardApplication(int lexsynNum, int lexsynTypeNum, int maxwords, int maxTypes) {
		super(SyntaxRuleType.ForwardApplication, lexsynNum, lexsynTypeNum, maxwords, maxTypes);
		
		String type = "A";
		String retLeft = "leftA";
		String retRight = "rightA";
		String retCombo = "comboA";
		
		List<ISlot> goalprecond = new ArrayList<ISlot>();
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new SlotVar(type)));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new NullValue()));
		goalprecond.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new NullValue()));

		this.requireGoal(goalprecond);
		
		List<ISlot> retprecond = new ArrayList<ISlot>();
		retprecond.add(new Slot(CCGTypeSlot.FullType, new SlotVar(type)));
		retprecond.add(new Slot(CCGTypeSlot.LeftType, new SlotVar(retLeft)));
		retprecond.add(new Slot(CCGTypeSlot.RightType, new SlotVar(retRight)));
		retprecond.add(new Slot(CCGTypeSlot.Combinator, new SlotVar(retCombo)));
		this.requireRetrieval(retprecond, ChunkTypeEnum.CCGType);
		
		List<ISlot> goalActions = new ArrayList<ISlot>();
		goalActions.add(new Slot(new SSlotName(SSlotNameEnum.LexsynLeftType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new SlotVar(retLeft)));
		goalActions.add(new Slot(new SSlotName(SSlotNameEnum.LexsynFullType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new SlotVar(type)));
		goalActions.add(new Slot(new SSlotName(SSlotNameEnum.LexsynRightType, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new SlotVar(retRight)));
		goalActions.add(new Slot(new SSlotName(SSlotNameEnum.LexsynCombo, lexsynNum, maxwords, lexsynTypeNum, maxTypes), new SlotVar(retCombo)));

		this.modifyGoal(goalActions);
	}

}
