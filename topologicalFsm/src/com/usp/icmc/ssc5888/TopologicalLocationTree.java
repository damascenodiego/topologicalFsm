package com.usp.icmc.ssc5888;

import java.util.ArrayList;
import java.util.List;

import com.usp.icmc.labes.fsm.FsmModel;
import com.usp.icmc.labes.fsm.FsmState;
import com.usp.icmc.labes.fsm.FsmTransition;

public class TopologicalLocationTree extends FsmModel{
	
	CurrentStateUncertainty singletonNodes;
	
	public TopologicalLocationTree() {
		this(null);
	}
	
	public TopologicalLocationTree(String n) {
		super(n);
	}
	
	


}
