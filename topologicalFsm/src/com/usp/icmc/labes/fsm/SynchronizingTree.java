package com.usp.icmc.labes.fsm;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import com.usp.icmc.ssc5888.CurrentStateUncertainty;

public class SynchronizingTree extends FsmModel{
	
	CurrentStateUncertainty singletonNodes;
	
	List<FsmTransition> closestSingleton;
	List<FsmTransition> farestSingleton;
	
	public SynchronizingTree() {
		super();
		closestSingleton = new LinkedList<FsmTransition>();
		farestSingleton = new LinkedList<FsmTransition>();
	}
	
	public SynchronizingTree(String n) {
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


	@Override
	public int hashCode() {
		int result = 1;
		return result;
	}
	@Override
	public boolean equals(Object obj) {
		return true;
	}
	
	
	
}
