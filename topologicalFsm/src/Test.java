import com.usp.icmc.labes.fsm.FsmModel;
import com.usp.icmc.labes.fsm.MazeUtils;
import com.usp.icmc.ssc5888.Maze;

import edu.princeton.cs.algs4.StdDraw;

public class Test {
	
	public static void main(String[] args) {
		
		
		Maze maze6x6 = new Maze(6);
		StdDraw.show(0);
		maze6x6.draw();

		MazeUtils utils = MazeUtils.getInstance();
		
		FsmModel topoMap = utils.extractTopologicalMap(maze6x6);
		
	}

}
