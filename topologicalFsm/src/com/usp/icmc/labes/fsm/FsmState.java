package com.usp.icmc.labes.fsm;

import java.util.ArrayList;
import java.util.List;

public class FsmState{

	private String id;
	
	private List<FsmTransition> in;
	private List<FsmTransition> out;
	
	public FsmState(String id_num) {
		id = id_num;
		in  = new ArrayList<FsmTransition>();
		out = new ArrayList<FsmTransition>();
	}
	
	
	
	public List<FsmTransition> getIn() {
		return in;
	}
	
	public List<FsmTransition> getOut() {
		return out;
	}

	@Override
	public String toString() {
		return id;
	}
	
	public String getId() {
		return id;
	}
	
	public void setId(String id) {
		this.id = id;
	}



	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((id == null) ? 0 : id.hashCode());
		result = prime * result + ((in == null) ? 0 : in.hashCode());
		result = prime * result + ((out == null) ? 0 : out.hashCode());
		return result;
	}



	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		FsmState other = (FsmState) obj;
		if (id == null) {
			if (other.id != null)
				return false;
		} else if (!id.equals(other.id))
			return false;
		if (in == null) {
			if (other.in != null)
				return false;
		} else if (!in.equals(other.in))
			return false;
		if (out == null) {
			if (other.out != null)
				return false;
		} else if (!out.equals(other.out))
			return false;
		return true;
	}

}
