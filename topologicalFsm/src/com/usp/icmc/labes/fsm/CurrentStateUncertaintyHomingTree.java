package com.usp.icmc.labes.fsm;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.usp.icmc.labes.fsm.FsmState;

public class CurrentStateUncertaintyHomingTree extends CurrentStateUncertainty{
	
	private Map<String,Set<FsmState>> uncertaintyMap;

	public CurrentStateUncertaintyHomingTree(String id) {
		super(id);
		uncertaintyMap = new HashMap<String,Set<FsmState>>();
	}
	
	
	public CurrentStateUncertaintyHomingTree() {
		this(null);
	}


	public Map<String,Set<FsmState>> getUncertaintyMap() {
		return uncertaintyMap;
	}


	@Override
	public int hashCode() {
		//final int prime = 31;
		int result = super.hashCode();
		//result = prime * result + ((uncertaintySet == null) ? 0 : uncertaintySet.hashCode());
		return result;
	}


	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		//if (!super.equals(obj))
		//	return false;
		if (getClass() != obj.getClass())
			return false;
		return true;
	}

	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		for (String k : uncertaintyMap.keySet()) {
			sb.append(uncertaintyMap.get(k));
			sb.append("_{"+k+"}\\n");
		}
		return sb.toString();
	}


	

}
