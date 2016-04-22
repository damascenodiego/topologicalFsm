package com.usp.icmc.labes.fsm;

import com.usp.icmc.ssc5888.Maze;

public class MazeUtils {

	private static MazeUtils instance;
	
	private MazeUtils() { }
	
	public static MazeUtils getInstance() {
		if(instance==null) instance = new MazeUtils();
		return instance;
	}
	
	public FsmModel extractTopologicalMap(Maze maze){
		
		FsmModel topoMap = new FsmModel();
		
		return topoMap;
	}
	
}
