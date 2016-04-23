package com.usp.icmc.labes.fsm;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import com.usp.icmc.ssc5888.Maze;
import com.usp.icmc.ssc5888.Robot;
import com.usp.icmc.ssc5888.Robot.Commands;

public class RobotUtils {

	public static FsmState updateTopologicalMap(Maze maze, int i, int j, FsmState curr) {

//		if (curr != null
//			&& 	Boolean.toString(maze.checkNorth(i,j)).equals(getTransition(curr, Robot.Commands.CHECK_NORTH).getOutput()) 	
//			&& 	Boolean.toString(maze.checkSouth(i,j)).equals(getTransition(curr, Robot.Commands.CHECK_SOUTH).getOutput())
//			&& 	Boolean.toString(maze.checkEast(i,j)).equals(getTransition(curr, Robot.Commands.CHECK_EAST).getOutput())
//			&& 	Boolean.toString(maze.checkWest(i,j)).equals(getTransition(curr, Robot.Commands.CHECK_WEST).getOutput())
//				){
//			return curr;
//		}

		String stateName = i+","+j;
		Robot robot = maze.getRobot();
		FsmState state = robot.getTopoMap().getState(stateName);

		if(state == null) {
			FsmState newState = new FsmState(stateName);
			robot.getTopoMap().addState(newState);
			state = robot.getTopoMap().getState(newState);
		}

		if (maze.checkNorth(i,j)){
						robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_NORTH.toString(), Boolean.TRUE.toString(), state));
						robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.MOVE_NORTH.toString(), Boolean.TRUE.toString(), state));
		}else {
			robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_NORTH.toString(), Boolean.FALSE.toString(), state));
		}

		if (maze.checkEast(i,j)){
						robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_EAST.toString(), Boolean.TRUE.toString(), state));
						robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.MOVE_EAST.toString(), Boolean.TRUE.toString(), state));
		}else{
			robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_EAST.toString(), Boolean.FALSE.toString(), state));
		}

		if (maze.checkSouth(i,j)) {
						robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_SOUTH.toString(), Boolean.TRUE.toString(), state));
						robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.MOVE_SOUTH.toString(), Boolean.TRUE.toString(), state));
		}else{
			robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_SOUTH.toString(), Boolean.FALSE.toString(), state));
		}
		if (maze.checkWest(i,j)) {
						robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_WEST.toString(), Boolean.TRUE.toString(), state));
						robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.MOVE_WEST.toString(), Boolean.TRUE.toString(), state));
		}else{
			robot.getTopoMap().addTransition(new FsmTransition(state, Robot.Commands.CHECK_WEST.toString(), Boolean.FALSE.toString(), state));
		}

		return state;
	}

	public static FsmTransition getTransition(FsmState st, Commands cmd){
		for (FsmTransition tr: st.getOut()) {
			if(tr.getInput().equals(cmd.toString())){
				return tr;
			}
		}
		return null;
	}

	public static void saveTopoMap(Robot r, File f) throws IOException{
		BufferedWriter bw = new BufferedWriter(new FileWriter(f));

		bw.write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><!--Created with JFLAP 6.4.--><structure>"); bw.write("\n");
		bw.write("	<type>mealy</type>");bw.write("\n");
		bw.write("	<automaton>");bw.write("\n");
		bw.write("		<!--The list of states.-->");bw.write("\n");

		int greaterx = 0;
		int greatery = 0;
		
		for (FsmState s: r.getTopoMap().getStates()) {
			String[] id = s.getId().split(",");
			int x = Integer.valueOf(id[0]);
			int y = Integer.valueOf(id[1]);
			if(greaterx < x) greaterx = x;
			if(greatery < y) greatery = y;
		}
		
		for (FsmState s: r.getTopoMap().getStates()) {
			bw.write("		<state id=\""+s.hashCode()+"\" name=\""+s.getId()+"\">");bw.write("\n");
			String[] id = s.getId().split(",");
			int x = Integer.valueOf(id[0]);
			int y = Integer.valueOf(id[1]);
			bw.write("			<x>"+(x*200)+"</x>"); bw.write("\n");
			bw.write("			<y>"+(greatery*200-y*200)+"</y>"); bw.write("\n");
			bw.write("		</state>");bw.write("\n");

		}

		bw.write("		<!--The list of transitions.-->");bw.write("\n");

		for (FsmTransition tr: r.getTopoMap().getTransitions()) {
			bw.write("		<transition>");bw.write("\n");
			bw.write("			<from>"+tr.getFrom().hashCode()+"</from>");bw.write("\n");
			bw.write("			<to>"+tr.getTo().hashCode()+"</to>");bw.write("\n");
			bw.write("			<read>"+tr.getInput()+"</read>");bw.write("\n");
			bw.write("			<transout>"+tr.getOutput()+"</transout>");bw.write("\n");
			bw.write("		</transition>");bw.write("\n");

		}

		bw.write("	</automaton>");bw.write("\n");
		bw.write("</structure>");

		bw.close();

	}
}
