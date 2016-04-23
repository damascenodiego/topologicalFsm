package com.usp.icmc.labes.fsm;

import com.usp.icmc.ssc5888.Maze;
import com.usp.icmc.ssc5888.Robot;

public class RobotUtils {

	public static FsmState updateTopologicalMap(Maze maze, int i, int j) {
		FsmState state = new FsmState(i+","+j);
		Robot robot = maze.getRobot();
		robot.getTopoMap().addState(state);
		state = robot.getTopoMap().getState(state);
		if (maze.checkNorth(i,j)){
			robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_NORTH.toString(), Boolean.TRUE.toString(), state));
		}
		if (maze.checkEast(i,j)){
			robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_EAST.toString(), Boolean.TRUE.toString(), state));
		}
		if (maze.checkSouth(i,j)) {
			robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_SOUTH.toString(), Boolean.TRUE.toString(), state));
		}
		if (maze.checkWest(i,j)) {
			robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_WEST.toString(), Boolean.TRUE.toString(), state));
		}
		
		return state;
	}

}
