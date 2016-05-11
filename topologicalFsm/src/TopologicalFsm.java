import java.io.File;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import com.usp.icmc.labes.fsm.FsmModel;
import com.usp.icmc.labes.fsm.MazeUtils;
import com.usp.icmc.labes.fsm.RobotUtils;
import com.usp.icmc.ssc5888.Maze;

import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.StdRandom;

public class TopologicalFsm {
	
	private static DefaultParser parser;
	private static Options options;

	private static final String 	N_PARAMETER 			= "n";
	private static final String 	SEED_PARAMETER 			= "seed";
	private static final String 	POS_PARAMETER 			= "pos";

	
	public static void main(String[] args) {
				
		
		try {
			int N = 2;
			long SEED = StdRandom.getSeed();
			Maze maze = null;
			
			int x = 1;
			int y = 1;
			
			parser = new DefaultParser();
			CommandLine cmd;
			setupCliOptions();
			
			cmd = parser.parse( options, args);
			if(cmd.hasOption(N_PARAMETER)) {
				N = Integer.valueOf(cmd.getOptionValue(N_PARAMETER));
			}
			if(cmd.hasOption(SEED_PARAMETER)) {
				SEED = Long.valueOf(cmd.getOptionValue(SEED_PARAMETER));
				StdRandom.setSeed(SEED);
			}
			if(cmd.hasOption(POS_PARAMETER)) {
				String coords[] = cmd.getOptionValue(POS_PARAMETER).split(",");
				x = Integer.valueOf(coords[0]); 
				y = Integer.valueOf(coords[1]);
				if((x<=0 || x>N) || (y<=0 || y>N)) {
					throw new Exception("Position ("+x+","+y+") out of bounds");
				}
			}

			System.out.println("Seed:\t"+StdRandom.getSeed());
			System.out.println("N:\t"+N);
			maze = new Maze(N,x,y);
			StdDraw.show(0);
			maze.draw();
			maze.solve();

			String fname = "topoMap_SEED_"+SEED+"_N_"+N;
			File folder = new File(fname+"/");
			folder.mkdirs();
			StdDraw.save(fname+"/"+fname+".png");
			RobotUtils.getInstance().saveTopoMap(maze, new File(folder,fname+".jff"));
			RobotUtils.getInstance().createHomingTree(maze);
			RobotUtils.getInstance().saveLocationTree(maze, new File(folder,fname+"_homingTree.jff"));
			RobotUtils.getInstance().saveLocationTreeAsDot(maze, new File(folder,fname+"_locationTree.dot"));
			//MazeUtils.getInstance().saveMaze(new File(folder,fname+"topomap.txt"), maze);
			
			StdDraw.text(0.5, 0.5, "TESTE!");
			
		} catch (Exception e1) {
			e1.printStackTrace();
		}

		
		

	}
	
	private static void setupCliOptions() {
		// create Options object
		options = new Options();
		

		Option nOption		 	= new Option(N_PARAMETER, 			true,	"size of the map. The map is squared.");
		Option seedOption 		= new Option(SEED_PARAMETER, 		true, 	"Custom seed. Random seed set as default.");
		Option posOption 		= new Option(POS_PARAMETER, 		true, 	"Robot custom position. Random position set as default.");
		

		options.addOption(nOption);
		options.addOption(seedOption);
		options.addOption(posOption);

	}

}
