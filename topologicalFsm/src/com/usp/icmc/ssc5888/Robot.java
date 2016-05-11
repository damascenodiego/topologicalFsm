package com.usp.icmc.ssc5888;

public class Robot {
	
	private String name;
	
	private TopologicalLocationTree topoTree;

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
		topoTree = new TopologicalLocationTree(getName()+"_LocationTree");
		
		for (Commands c : Commands.values()) {
			topoTree.addInput(c.toString());
		}
		
		topoTree.addOutputs(Boolean.TRUE.toString());
		topoTree.addOutputs(Boolean.FALSE.toString());
		
	}

	public String getName() {
		return name;
	}
	
	public TopologicalLocationTree getSyncTree() {
		return topoTree;
	}
	
	public TopologicalMap getTopoMap() {
		return topoMap;
	}
	
}
