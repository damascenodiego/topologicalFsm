package com.usp.icmc.ssc5888;

public class Robot {
	
	private String name;
	
	private SynchronizationTree syncTree;

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
		topoMap  = new TopologicalMap(getName()+"_SynchronizationTopologicalMap");
		
		for (Commands c : Commands.values()) {
			topoMap.addInput(c.toString());
		}
		
		topoMap.addOutputs(Boolean.TRUE.toString());
		topoMap.addOutputs(Boolean.FALSE.toString());
		
	}

	private void setupsyncTree() {
		syncTree = new SynchronizationTree(getName()+"_SynchronizationTree");
		
		for (Commands c : Commands.values()) {
			syncTree.addInput(c.toString());
		}
		
		syncTree.addOutputs(Boolean.TRUE.toString());
		syncTree.addOutputs(Boolean.FALSE.toString());
		
	}

	public String getName() {
		return name;
	}
	
	public SynchronizationTree getSyncTree() {
		return syncTree;
	}
	
	public TopologicalMap getTopoMap() {
		return topoMap;
	}
	
}
