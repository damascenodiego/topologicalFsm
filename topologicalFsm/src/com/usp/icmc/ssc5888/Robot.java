package com.usp.icmc.ssc5888;

import java.util.Collections;

import com.usp.icmc.labes.fsm.FsmModel;
import com.usp.icmc.labes.fsm.HomingTree;

public class Robot {
	
	private String name;
	
	private FsmModel locationTree;

	private TopologicalMap topoMap;
	
	public Robot() {
		this("noname");
	}
	
	
	public enum Commands {
		CHECK_NORTH,	CHECK_SOUTH,	CHECK_WEST,	CHECK_EAST,
		MOVE_NORTH,		MOVE_SOUTH,		MOVE_WEST,	MOVE_EAST,};
	
	
	public Robot(String name){
		this.name = name;
		
		setupsyncTree();
		topoMapTree();
		
	}
	
	private void topoMapTree() {
		topoMap  = new TopologicalMap(getName()+"_TopologicalMap");
		
		for (Commands c : Commands.values()) {
			topoMap.addInput(c.toString());
		}
		Collections.sort(topoMap.getInputs());
		topoMap.addOutputs(Boolean.TRUE.toString());
		topoMap.addOutputs(Boolean.FALSE.toString());
		
	}

	private void setupsyncTree() {
		locationTree = new HomingTree(getName()+"_HomingTree");
		
		for (Commands c : Commands.values()) {
			locationTree.addInput(c.toString());
		}
		
		locationTree.addOutputs(Boolean.TRUE.toString());
		locationTree.addOutputs(Boolean.FALSE.toString());
		
	}

	public String getName() {
		return name;
	}
	
	public FsmModel getLocationTree() {
		return locationTree;
	}
	
	public void setLocationTree(FsmModel locationTree) {
		this.locationTree = locationTree;
	}
	
	public TopologicalMap getTopoMap() {
		return topoMap;
	}
	
}
