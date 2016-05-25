package com.usp.icmc.labes.fsm;

import java.util.ArrayList;
import java.util.List;

public abstract class FsmModel{

	String name;
	
	List<FsmState> states;
	FsmState initialState;
	List<FsmTransition> transitions;
	List<String> inputs;
	List<String> outputs;

	public FsmModel() {
		this.states = new ArrayList<FsmState>();
		this.transitions = new ArrayList<FsmTransition>();
		this.inputs = new ArrayList<String>();
		this.outputs = new ArrayList<String>();
	}
	public FsmModel(String n) {
		this();
		this.name = n;
	}

	public boolean addInput(String in){
		if(!this.inputs.contains(in)){
			this.inputs.add(in);
			return true;
		}
		return false;
	}

	public boolean addOutputs(String out){
		if(!this.outputs.contains(out)){
			this.outputs.add(out);
			return true;
		}
		return false;
	}

	public void addState(FsmState el){
		if(!this.states.contains(el)) this.states.add(el);			
		if(initialState==null) this.initialState = el;
	}

	public void addTransition(FsmTransition el){
		if(!this.transitions.contains(el)){
			this.transitions.add(el);
			addState(el.getFrom());
			addState(el.getTo());
			addInput(el.getInput());
			addOutputs(el.getOutput());
		}
	}

	public FsmState getInitialState() {
		return initialState;
	}
	
	public List<String> getInputs() {
		return inputs;
	}

	
	public List<String> getOutputs() {
		return outputs;
	}
	
	public FsmState getState(FsmState s) {
		for (FsmState fsmState : states) {
			if(fsmState.equals(s)) 
				return fsmState;
		}
		return null;
	}
	
	public FsmState getState(String ident) {
		for (FsmState fsmState : states) {
			if(fsmState.getId().equals(ident)){
				return fsmState;
			}
		}
		return null;
	}
	
	public List<FsmState> getStates() {
		return this.states;
	}

	public List<FsmTransition> getTransitions() {
		return this.transitions;
	}
	
	public void setInitialState(FsmState initialState) {
		this.initialState = initialState;
	}

	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	@Override
	public abstract int hashCode();
	
	@Override
	public abstract boolean equals(Object obj);
	
	
}
