package com.usp.icmc.ssc5888;

public class Robot {
	
	private String name;
	
	private TopologicalLocationTree locationTree;

	private TopologicalMap topoMap;
	
	public Robot() {
		this("noname");
	}
	
	
	public enum Commands {
		MOVE_NORTH,		MOVE_SOUTH,		MOVE_WEST,	MOVE_EAST,
		CHECK_NORTH,	CHECK_SOUTH,	CHECK_WEST,	CHECK_EAST};
	
	
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
		
		topoMap.addOutputs(Boolean.TRUE.toString());
		topoMap.addOutputs(Boolean.FALSE.toString());
		
	}

	private void setupsyncTree() {
		locationTree = new TopologicalLocationTree(getName()+"_LocationTree");
		
		for (Commands c : Commands.values()) {
			locationTree.addInput(c.toString());
		}
		
		locationTree.addOutputs(Boolean.TRUE.toString());
		locationTree.addOutputs(Boolean.FALSE.toString());
		
	}

	public String getName() {
		return name;
	}
	
	public TopologicalLocationTree getLocationTree() {
		return locationTree;
	}
	
	public TopologicalMap getTopoMap() {
		return topoMap;
	}
	
}
