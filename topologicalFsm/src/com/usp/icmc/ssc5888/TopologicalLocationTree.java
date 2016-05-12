package com.usp.icmc.ssc5888;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.usp.icmc.labes.fsm.FsmModel;
import com.usp.icmc.labes.fsm.FsmState;
import com.usp.icmc.labes.fsm.FsmTransition;

public class TopologicalLocationTree extends FsmModel{
	
	CurrentStateUncertainty singletonNodes;
	
	List<FsmTransition> closestSingleton;
	List<FsmTransition> farestSingleton;
	
	public TopologicalLocationTree() {
		this(null);
	}
	
	public TopologicalLocationTree(String n) {
		super(n);
		closestSingleton = new LinkedList<FsmTransition>();
		farestSingleton = new LinkedList<FsmTransition>();
	}
	
	
	public List<FsmTransition> getClosestSingleton() {
		return closestSingleton;
	}
	
	public List<FsmTransition> getFarestSingleton() {
		return farestSingleton;
	}
	
	public void setClosestSingleton(List<FsmTransition> closestSingleton) {
		this.closestSingleton = closestSingleton;
	}
	
	public void setFarestSingleton(List<FsmTransition> farestSingleton) {
		this.farestSingleton = farestSingleton;
	}

}
