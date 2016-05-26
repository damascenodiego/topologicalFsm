package com.usp.icmc.labes.fsm;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class HomingTree extends FsmModel implements ICurrentStateUncertaintyTree{
	
	CurrentStateUncertaintyHomingTree singletonNodes;
	
	List<FsmTransition> closestSingleton;
	List<FsmTransition> farestSingleton;
	
	List<FsmTransition> closestAllSingleton;
	List<FsmTransition> farestAllSingleton;
	
	public HomingTree() {
		super();
		closestSingleton = new LinkedList<FsmTransition>();
		farestSingleton = new LinkedList<FsmTransition>();
	}
	
	public HomingTree(String n) {
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

	
	public List<FsmTransition> getClosestAllSingleton() {
		return closestAllSingleton;
	}
	
	public List<FsmTransition> getFarestAllSingleton() {
		return farestAllSingleton;
	}

	public void setClosestAllSingleton(List<FsmTransition> closestAllSingleton) {
		this.closestAllSingleton = closestAllSingleton;
	}
	
	public void setFarestAllSingleton(List<FsmTransition> farestAllSingleton) {
		this.farestAllSingleton = farestAllSingleton;
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
