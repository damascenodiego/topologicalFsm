package com.usp.icmc.ssc5888;

import java.util.HashSet;
import java.util.Set;

import com.usp.icmc.labes.fsm.FsmState;

public class CurrentStateUncertainty extends FsmState {
	
	private Set<FsmState> uncertaintySet;

	public CurrentStateUncertainty(int id_num) {
		super(id_num);
		uncertaintySet = new HashSet<>();
	}
	
	
	public Set<FsmState> getUncertaintySet() {
		return uncertaintySet;
	}
	

}
