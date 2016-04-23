package com.usp.icmc.ssc5888;

import java.util.ArrayList;
import java.util.List;

import com.usp.icmc.labes.fsm.FsmModel;
import com.usp.icmc.labes.fsm.FsmState;
import com.usp.icmc.labes.fsm.FsmTransition;

public class SynchronizationTree extends FsmModel{
	
	
	String name;
	
	List<FsmState> states;
	FsmState initialState;
	List<FsmTransition> transitions;
	List<String> inputs;
	List<String> outputs;

	public SynchronizationTree() {
		this.states = new ArrayList<FsmState>();
		this.transitions = new ArrayList<FsmTransition>();
		this.inputs = new ArrayList<String>();
		this.outputs = new ArrayList<String>();
	}
	
	public SynchronizationTree(String n) {
		this();
		this.name=n;
	}


}
