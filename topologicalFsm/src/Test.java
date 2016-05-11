import java.io.File;

import com.usp.icmc.labes.fsm.FsmModel;
import com.usp.icmc.labes.fsm.MazeUtils;
import com.usp.icmc.labes.fsm.RobotUtils;
import com.usp.icmc.ssc5888.Maze;

import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.StdRandom;

public class Test {

	public static void main(String[] args) {
		int N = 5;
		int SEED = 1000;
		if(args.length==1) N = Integer.parseInt(args[0]);
		if(args.length==2){
			N = Integer.parseInt(args[0]);
			SEED = Integer.parseInt(args[1]);
		}

		StdRandom.setSeed(SEED);
		Maze maze = new Maze(N);
		StdDraw.show(0);
		maze.draw();
		maze.solve();

		try {
			String fname = "test";
			//StdDraw.save(fname+".png");
			RobotUtils.getInstance().saveTopoMap(maze.getRobot(), new File(fname+"_topomap.jff"));
			RobotUtils.getInstance().createHomingTree(maze.getRobot());
			//RobotUtils.getInstance().saveSyncTree(maze.getRobot(), new File(fname+"_syncTree.jff"));
			RobotUtils.getInstance().saveSyncTreeAsDot(maze.getRobot(), new File(fname+"_syncTree.dot"));
			System.exit(0);
		} catch (Exception e) {
			e.printStackTrace();
			System.exit(1);
		}

	}

}
