package com.usp.icmc.labes.fsm;

import java.util.List;

public interface ICurrentStateUncertaintyTree {

	public List<FsmTransition> getClosestSingleton();

	public List<FsmTransition> getFarestSingleton();
	
	public List<FsmTransition> getClosestAllSingleton();

	public List<FsmTransition> getFarestAllSingleton();
	
}
