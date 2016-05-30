package com.usp.icmc.ssc5888;

import java.awt.Color;
import java.awt.Font;

import com.usp.icmc.labes.fsm.CurrentStateUncertainty;
import com.usp.icmc.labes.fsm.CurrentStateUncertaintyHomingTree;
import com.usp.icmc.labes.fsm.DrawUtils;
import com.usp.icmc.labes.fsm.FsmState;
import com.usp.icmc.labes.fsm.FsmTransition;
import com.usp.icmc.labes.fsm.ICurrentStateUncertainty;
import com.usp.icmc.ssc5888.Robot.Commands;

import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.StdRandom;

/******************************************************************************
 *  Compilation:  javac Maze.java
 *  Execution:    java Maze.java N
 *  Dependencies: StdDraw.java
 *
 *  Generates a perfect N-by-N maze using depth-first search with a stack.
 *
 *  % java Maze 62
 *
 *  % java Maze 61
 *
 *  Note: this program generalizes nicely to finding a random tree
 *        in a graph.
 *
 ******************************************************************************/

public class Maze {
	private int N;                 // dimension of maze
	private boolean[][] north;     // is there a wall to north of cell i, j
	private boolean[][] east;
	private boolean[][] south;
	private boolean[][] west;
	private boolean[][] visited;
	private int robotX;
	private int robotY;
	Robot robot;

	private boolean done = false;
	private Font smallFont;
	private Font tinyFont;
	private Font defaultFont;

	private static final int initialX = 1;
	private static final int initialY = 1;


	long seed;

	public Maze(int N, int x, int y) {
		this.N = N;

		if(DrawUtils.getInstance().getShowWindow()){
			defaultFont = StdDraw.getFont();
			smallFont = new Font("Arial", Font.PLAIN, 9);
			tinyFont = new Font("Arial", Font.PLAIN, 7);

			StdDraw.setXscale(0, N+2);
			StdDraw.setYscale(0, N+2);
		}
		robot = new Robot();
		robotX = x;
		robotY = y;
		init();
		generate();

	}

	public Maze(int N) {
		this(N, initialX, initialY);
	}

	public boolean checkNorth(int i,int j){
		return north[i][j];
	}
	public boolean checkSouth(int i,int j){
		return south[i][j];
	}
	public boolean checkWest(int i,int j){
		return west[i][j];
	}
	public boolean checkEast(int i,int j){
		return east[i][j];
	}

	private void init() {
		// initialize border cells as already visited
		visited = new boolean[N+2][N+2];
		for (int x = 0; x < N+2; x++) {
			visited[x][0] = true;
			visited[x][N+1] = true;
		}
		for (int y = 0; y < N+2; y++) {
			visited[0][y] = true;
			visited[N+1][y] = true;
		}


		// initialze all walls as present
		north = new boolean[N+2][N+2];
		east  = new boolean[N+2][N+2];
		south = new boolean[N+2][N+2];
		west  = new boolean[N+2][N+2];
		for (int x = 0; x < N+2; x++) {
			for (int y = 0; y < N+2; y++) {
				north[x][y] = true;
				east[x][y]  = true;
				south[x][y] = true;
				west[x][y]  = true;
			}
		}
	}


	public Robot getRobot() {
		return robot;
	}


	public long getSeed() {
		return seed;
	}

	// generate the maze
	private void generate(int x, int y) {
		seed = StdRandom.getSeed();
		visited[x][y] = true;

		// while there is an unvisited neighbor
		while (!visited[x][y+1] || !visited[x+1][y] || !visited[x][y-1] || !visited[x-1][y]) 
		{

			// pick random neighbor (could use Knuth's trick instead)
			while (true) {
				double r = StdRandom.uniform(4);
				if (r == 0 && !visited[x][y+1]) {
					north[x][y] = false;
					south[x][y+1] = false;
					generate(x, y + 1);
					break;
				}
				else if (r == 1 && !visited[x+1][y]) {
					east[x][y] = false;
					west[x+1][y] = false;
					generate(x+1, y);
					break;
				}
				else if (r == 2 && !visited[x][y-1]) {
					south[x][y] = false;
					north[x][y-1] = false;
					generate(x, y-1);
					break;
				}
				else if (r == 3 && !visited[x-1][y]) {
					west[x][y] = false;
					east[x-1][y] = false;
					generate(x-1, y);
					break;
				}
			}
		}
	}


	// generate the maze starting from lower left
	private void generate() {
		generate(robotX, robotY);
	}

	// solve the maze using depth-first search
	private void solve(int x, int y, FsmState curr) {
		if (x == 0 || y == 0 || x == N+1 || y == N+1) return;
		if (done || visited[x][y]) return;
		visited[x][y] = true;

		//StdDraw.setPenColor(StdDraw.BLUE);
		//StdDraw.filledCircle(x + 0.5, y + 0.5, 0.25);
		//StdDraw.show(30);

		// reached middle
		//if (x == N/2 && y == N/2) done = true;

		FsmState next = null; 
		FsmState currState = robot.getTopoMap().getState(curr);
		if (!north[x][y]) {
			next = RobotUtils.getInstance().updateTopologicalMap(this, x, y+1,currState);
			if(!currState.equals(next)) robot.getTopoMap().addTransition(new FsmTransition(currState, Robot.Commands.MOVE_NORTH.toString(), Boolean.FALSE.toString(), robot.getTopoMap().getState(next)));
			solve(x, y + 1,next);
		}
		if (!east[x][y])  {
			next = RobotUtils.getInstance().updateTopologicalMap(this, x+1, y,currState);
			if(!currState.equals(next)) robot.getTopoMap().addTransition(new FsmTransition(robot.getTopoMap().getState(curr), Robot.Commands.MOVE_EAST.toString(), Boolean.FALSE.toString(), robot.getTopoMap().getState(next)));
			solve(x + 1, y,next);
		}
		if (!south[x][y]) {
			next = RobotUtils.getInstance().updateTopologicalMap(this, x, y-1,currState);
			if(!currState.equals(next)) robot.getTopoMap().addTransition(new FsmTransition(robot.getTopoMap().getState(curr), Robot.Commands.MOVE_SOUTH.toString(), Boolean.FALSE.toString(), robot.getTopoMap().getState(next)));
			solve(x, y - 1,next);
		}
		if (!west[x][y])  {
			next = RobotUtils.getInstance().updateTopologicalMap(this, x-1, y,currState);
			if(!currState.equals(next)) robot.getTopoMap().addTransition(new FsmTransition(robot.getTopoMap().getState(curr), Robot.Commands.MOVE_WEST.toString(), Boolean.FALSE.toString(), robot.getTopoMap().getState(next)));
			solve(x - 1, y,next);
		}

		//if (done) return;

		//StdDraw.setPenColor(StdDraw.GRAY);
		//StdDraw.filledCircle(x + 0.5, y + 0.5, 0.25);
		//StdDraw.show(30);
	}

	// solve the maze starting from the start state
	public void solve() {
		for (int x = 1; x <= N; x++)
			for (int y = 1; y <= N; y++)
				visited[x][y] = false;
		done = false;


		int i = 1;
		int j = 1;

		FsmState state  = RobotUtils.getInstance().updateTopologicalMap(this, i, j,null);

		solve(i, j,state);
	}

	// draw the maze
	public void draw() {
		if(!DrawUtils.getInstance().getShowWindow()) return;
		fillCurrent();
		
		StdDraw.setPenColor(StdDraw.BLACK);
		for (int x = 1; x <= N; x++) {
			for (int y = 1; y <= N; y++) {
				if (south[x][y]) StdDraw.line(x, y, x + 1, y);
				if (north[x][y]) StdDraw.line(x, y + 1, x + 1, y + 1);
				if (west[x][y])  StdDraw.line(x, y, x, y + 1);
				if (east[x][y])  StdDraw.line(x + 1, y, x + 1, y + 1);
				StdDraw.setFont(smallFont);
				StdDraw.text(x, y, x+","+y);
				StdDraw.setFont(defaultFont);
			}
		}
//		StdDraw.show(1000);
	}

	public void fillCurrent() {
		if(!DrawUtils.getInstance().getShowWindow()) return;

		StdDraw.setPenColor(StdDraw.BLUE);
		int x = getRobotX(); 
		int y = getRobotY();
		StdDraw.filledCircle(x+.5,y+.5, 0.275);
//		StdDraw.show(1000);
	}

	public void writeText(double x, double y, String txt,Color col,Font f){
		if(!DrawUtils.getInstance().getShowWindow()) return;
		StdDraw.setPenColor(col);
		StdDraw.setFont(f);
		StdDraw.text(x, y, txt);
		StdDraw.setFont(defaultFont);
//		StdDraw.show(1000);
	}
	
	public void writeText(double x, double y, String txt){
		if(!DrawUtils.getInstance().getShowWindow()) return;
		StdDraw.setPenColor(StdDraw.MAGENTA);
		StdDraw.setFont(smallFont);
		StdDraw.text(x, y, txt);
		StdDraw.setFont(defaultFont);
//		StdDraw.show(1000);
	}

	
	public void fillCurrent(ICurrentStateUncertainty csu) {
		if(!DrawUtils.getInstance().getShowWindow()) return;
		for (FsmState un : csu.getUncertaintySet()) {
			StdDraw.setPenColor(StdDraw.RED);
			String coords[] = un.getId().split(",");
			int x = Integer.valueOf(coords[0]); 
			int y = Integer.valueOf(coords[1]);
			StdDraw.filledCircle(x+.5,y+.5, 0.375);
		}
//		StdDraw.show(1000);
	}
	public void eraseCurrent(ICurrentStateUncertainty csu) {
		if(!DrawUtils.getInstance().getShowWindow()) return;

		for (FsmState un : csu.getUncertaintySet()) {
			StdDraw.setPenColor(StdDraw.WHITE);
			String coords[] = un.getId().split(",");
			int x = Integer.valueOf(coords[0]); 
			int y = Integer.valueOf(coords[1]);
			StdDraw.filledCircle(x+.5,y+.5, 0.45);
		}
//		StdDraw.show(1000);
	}
	
	public void fillCurrent(CurrentStateUncertaintyHomingTree csu, String key) {
		if(!DrawUtils.getInstance().getShowWindow()) return;
		for (FsmState un : csu.getUncertaintyMap().get(key)) {
			StdDraw.setPenColor(StdDraw.RED);
			String coords[] = un.getId().split(",");
			int x = Integer.valueOf(coords[0]); 
			int y = Integer.valueOf(coords[1]);
			StdDraw.filledCircle(x+.5,y+.5, 0.375);
		}
//		StdDraw.show(1000);
	}
	
	public void eraseCurrent(CurrentStateUncertaintyHomingTree csu, String key) {
		if(!DrawUtils.getInstance().getShowWindow()) return;

		for (FsmState un : csu.getUncertaintyMap().get(key)) {
			StdDraw.setPenColor(StdDraw.WHITE);
			String coords[] = un.getId().split(",");
			int x = Integer.valueOf(coords[0]); 
			int y = Integer.valueOf(coords[1]);
			StdDraw.filledCircle(x+.5,y+.5, 0.45);
		}
//		StdDraw.show(1000);
	}
	
	private int getRobotX() {
		return robotX;
	}
	private int getRobotY() {
		return robotY;
	}
	
	private boolean setRobotXY(int x, int y) {
		if (x == 0 || y == 0 || x == N+1 || y == N+1) {
			return true;
		}
		
		this.robotX = x;
		this.robotY = y;
		
		return false;
	}

	public boolean moveRobotNorth(){
		int x = getRobotX();
		int y = getRobotY()+1;
		if (!checkRobotNorth()) {
			return setRobotXY(x,y);
		}
		return true;
	}
	
	public boolean moveRobotEast(){
		int x = getRobotX()+1;
		int y = getRobotY();
		if (!checkRobotEast())  {
			return setRobotXY(x,y);
		}
		return true;
	}
	
	public boolean moveRobotSouth(){
		int x = getRobotX();
		int y = getRobotY()-1;
		if (!checkRobotSouth()) {
			return setRobotXY(x,y);
		}
		return true;
	}
	
	public boolean moveRobotWest(){
		int x = getRobotX()-1;
		int y = getRobotY();
		if (!checkRobotWest())  {
			return setRobotXY(x,y);
		}
		return true;
	}

	public boolean checkRobotNorth(){
		int x = getRobotX();
		int y = getRobotY();
		return (north[x][y]);
	}
	
	public boolean checkRobotEast(){
		int x = getRobotX();
		int y = getRobotY();
		return (east[x][y]);
	}
	
	public boolean checkRobotSouth(){
		int x = getRobotX();
		int y = getRobotY();
		return (south[x][y]);
	}
	
	public boolean checkRobotWest(){
		int x = getRobotX();
		int y = getRobotY();
		return (west[x][y]);
	}

	public boolean[][] getNorth() {
		return north;
	}

	public boolean[][] getSouth() {
		return south;
	}

	public boolean[][] getWest() {
		return west;
	}

	public boolean[][] getEast() {
		return east;
	}

	public int getN() {
		return N;
	}

	
	public Font getDefaultFont() {
		return defaultFont;
	}
	
	public Font getSmallFont() {
		return smallFont;
	}
	
	public Font getTinyFont() {
		return tinyFont;
	}

	public boolean runCommand(String input) {
		if(input.equals(Commands.CHECK_NORTH.toString())) 	return checkRobotNorth();
		if(input.equals(Commands.CHECK_EAST.toString())) 	return checkRobotEast();
		if(input.equals(Commands.CHECK_SOUTH.toString())) 	return checkRobotSouth();
		if(input.equals(Commands.CHECK_WEST.toString())) 	return checkRobotWest();
		
		if(input.equals(Commands.MOVE_NORTH.toString())) 	return moveRobotNorth();
		if(input.equals(Commands.MOVE_EAST.toString())) 	return moveRobotEast();
		if(input.equals(Commands.MOVE_SOUTH.toString())) 	return moveRobotSouth();
		if(input.equals(Commands.MOVE_WEST.toString())) 	return moveRobotWest();
		
		return false;
	}
}

