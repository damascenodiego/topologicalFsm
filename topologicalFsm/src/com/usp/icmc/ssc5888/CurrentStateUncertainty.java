package com.usp.icmc.ssc5888;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.usp.icmc.labes.fsm.FsmState;

public class CurrentStateUncertainty extends FsmState {
	
	private List<FsmState> uncertaintySet;

	public CurrentStateUncertainty(String id) {
		super(id);
		uncertaintySet = new ArrayList<>();
	}
	
	
	public CurrentStateUncertainty() {
		this(null);
	}


	public List<FsmState> getUncertaintySet() {
		return uncertaintySet;
	}


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((uncertaintySet == null) ? 0 : uncertaintySet.hashCode());
		return result;
	}


	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		CurrentStateUncertainty other = (CurrentStateUncertainty) obj;
		if (uncertaintySet == null) {
			if (other.uncertaintySet != null)
				return false;
		} else if (!uncertaintySet.equals(other.uncertaintySet))
			return false;
		return true;
	}



	

}
