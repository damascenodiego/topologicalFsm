package com.usp.icmc.ssc5888;

import java.awt.Font;
import java.io.File;

import com.usp.icmc.labes.fsm.FsmState;
import com.usp.icmc.labes.fsm.FsmTransition;
import com.usp.icmc.labes.fsm.RobotUtils;
import com.usp.icmc.labes.fsm.testing.FsmSUT;

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
	private Font defaultFont;

	private static final int initialX = 1;
	private static final int initialY = 1;

	public Maze(int N) {
		defaultFont = StdDraw.getFont();
		smallFont = new Font("Arial", Font.PLAIN, 9);

		this.N = N;
		StdDraw.setXscale(0, N+2);
		StdDraw.setYscale(0, N+2);
		robot = new Robot();
		robotX = initialX;
		robotY = initialY;
		init();
		generate();

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

	// generate the maze
	private void generate(int x, int y) {
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
			next = RobotUtils.updateTopologicalMap(this, x, y+1,currState);
			if(!currState.equals(next)) robot.getTopoMap().addTransition(new FsmTransition(currState, Robot.Commands.MOVE_NORTH.toString(), Boolean.FALSE.toString(), robot.getTopoMap().getState(next)));
			solve(x, y + 1,next);
		}
		if (!east[x][y])  {
			next = RobotUtils.updateTopologicalMap(this, x+1, y,currState);
			if(!currState.equals(next)) robot.getTopoMap().addTransition(new FsmTransition(robot.getTopoMap().getState(curr), Robot.Commands.MOVE_EAST.toString(), Boolean.FALSE.toString(), robot.getTopoMap().getState(next)));
			solve(x + 1, y,next);
		}
		if (!south[x][y]) {
			next = RobotUtils.updateTopologicalMap(this, x, y-1,currState);
			if(!currState.equals(next)) robot.getTopoMap().addTransition(new FsmTransition(robot.getTopoMap().getState(curr), Robot.Commands.MOVE_SOUTH.toString(), Boolean.FALSE.toString(), robot.getTopoMap().getState(next)));
			solve(x, y - 1,next);
		}
		if (!west[x][y])  {
			next = RobotUtils.updateTopologicalMap(this, x-1, y,currState);
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

		FsmState state  = RobotUtils.updateTopologicalMap(this, i, j,null);

		solve(i, j,state);
	}

	// draw the maze
	public void draw() {
		//fillCurrent();

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
		StdDraw.show(1000);
	}



	private void fillCurrent() {
		StdDraw.setPenColor(StdDraw.RED);
		StdDraw.text(robotX+.5,robotY+.5, "⟹",0);
		//StdDraw.filledCircle(current_x+.5,current_y+.5, 0.375);
		StdDraw.show(1000);
	}
	private void eraseCurrent() {
		StdDraw.setPenColor(StdDraw.WHITE);
		StdDraw.filledSquare(robotX+.5,robotY+.5, 0.45);
		//StdDraw.filledCircle(current_x+.5,current_y+.5, 0.38);
		StdDraw.show(1000);
	}



	// a test client
	public static void main(String[] args) {
		int N 
		= 10; 
		//= Integer.parseInt(args[0]);
		Maze maze = new Maze(N);
		StdDraw.show(0);
		maze.draw();
		maze.solve();
		//maze.eraseCurrent();

		try {
			File f = new File("test.jff");
			StdDraw.save("test.png");
			RobotUtils.saveTopoMap(maze.getRobot(), f);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}

