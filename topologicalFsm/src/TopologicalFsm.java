import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import com.usp.icmc.labes.fsm.DrawUtils;
import com.usp.icmc.labes.fsm.FsmTransition;
import com.usp.icmc.labes.fsm.MazeUtils;
import com.usp.icmc.labes.fsm.RobotUtils;
import com.usp.icmc.ssc5888.CurrentStateUncertainty;
import com.usp.icmc.ssc5888.Maze;

import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.StdRandom;

public class TopologicalFsm {

	private static DefaultParser parser;
	private static Options options;

	private static final String 	N_PARAMETER 			= "n";
	private static final String 	SEED_PARAMETER 			= "seed";
	private static final String 	POS_PARAMETER 			= "pos";
	private static final String 	SAVE_PARAMETER 			= "save";
	private static final String 	SHOW_WINDOW_PARAMETER	= "window";


	public static void main(String[] args) {


		try {
			int N = 3;
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

			if(!cmd.hasOption(SHOW_WINDOW_PARAMETER)) {
				DrawUtils.getInstance().setShowWindow(false);
			}
			System.out.println("Seed:\t"+StdRandom.getSeed());
			System.out.println("N:\t"+N);
			maze = new Maze(N,x,y);
			maze.solve();

			String fname = "topoMap_SEED_"+SEED+"_N_"+N;
			RobotUtils.getInstance().createHomingTree(maze);

			System.out.println("ClosestSingleton:\t"+maze.getRobot().getLocationTree().getClosestSingleton().size());
			System.out.println("FarestSingleton:\t"+maze.getRobot().getLocationTree().getFarestSingleton().size());

			if(cmd.hasOption(SAVE_PARAMETER)){
				File folder = new File(new File("topologicalFsm"),fname+"/");
				folder.mkdirs();
				if(DrawUtils.getInstance().getShowWindow()) {
					StdDraw.show(0);
					maze.draw();
					StdDraw.save(new File(folder,fname+".png").getAbsolutePath());
				}
				RobotUtils.getInstance().saveTopoMap(maze, new File(folder,fname+".jff"));
				RobotUtils.getInstance().saveLocationTree(maze, new File(folder,fname+"_locationTree.jff"));
				RobotUtils.getInstance().saveLocationTreeAsDot(maze, new File(folder,fname+"_locationTree.dot"));
				MazeUtils.getInstance().saveMaze(new File(folder,fname+"topomap.txt"), maze);
				BufferedWriter bw = new BufferedWriter(new FileWriter(new File(folder,"singletons.txt")));

				bw.write("ClosestSingleton:\n");
				for (FsmTransition tr : maze.getRobot().getLocationTree().getClosestSingleton()) {
					bw.write( "\t" +
							//							((CurrentStateUncertainty)tr.getFrom()).getUncertaintySet() +
							//						 	 " -- "+
							tr.getInput() +
							" / " +
							tr.getOutput()+
							//						    " -> " +
							//						 	((CurrentStateUncertainty)tr.getTo()).getUncertaintySet() +
							"\n"
							);
					//bw.write("ClosestSingleton:\n");
				}

				bw.write("FarestSingleton:\n");
				for (FsmTransition tr : maze.getRobot().getLocationTree().getFarestSingleton()) {
					bw.write( "\t" +
							//							((CurrentStateUncertainty)tr.getFrom()).getUncertaintySet() +
							//						 	 " -- "+
							tr.getInput() +
							" / " +
							tr.getOutput()+
							//						    " -> " +
							//						 	((CurrentStateUncertainty)tr.getTo()).getUncertaintySet() +
							"\n"
							);
					//bw.write("ClosestSingleton:\n");
				}
				bw.close();
				
				if(DrawUtils.getInstance().getShowWindow()) {
					int count = -1;
					CurrentStateUncertainty csu = null;
					for (FsmTransition tr : maze.getRobot().getLocationTree().getClosestSingleton()) {
						csu = (CurrentStateUncertainty) tr.getFrom();
						StdDraw.clear();
						maze.fillCurrent(csu);
						maze.writeText(((maze.getN()+2)/2), 0.5, tr.getInput() + " / " + tr.getOutput());
						maze.draw();
						StdDraw.save(new File(folder,fname+"_step_"+(++count)+".png").getAbsolutePath());
						csu = (CurrentStateUncertainty) tr.getTo();
						StdDraw.clear();
						maze.fillCurrent(csu);
						maze.writeText(((maze.getN()+2)/2), 0.5, tr.getInput() + " / " + tr.getOutput());
						maze.draw();
						StdDraw.save(new File(folder,fname+"_step_"+(++count)+".png").getAbsolutePath());
						
					}
				}
			}

		} catch (Exception e1) {
			e1.printStackTrace();
			System.exit(1);
		}
	}

	private static void setupCliOptions() {
		// create Options object
		options = new Options();


		Option nOption		 	= new Option(N_PARAMETER, 			true,	"size of the map. The map is squared.");
		Option seedOption 		= new Option(SEED_PARAMETER, 		true, 	"Custom seed. Random seed set as default.");
		Option posOption 		= new Option(POS_PARAMETER, 		true, 	"Robot custom position. Random position set as default.");
		Option saveDataOption 	= new Option(SAVE_PARAMETER, 		false, 	"Saves data generated.");
		Option showWindowOption = new Option(SHOW_WINDOW_PARAMETER, false, 	"Show window.");


		options.addOption(nOption);
		options.addOption(seedOption);
		options.addOption(posOption);
		options.addOption(saveDataOption);
		options.addOption(showWindowOption);

	}

}
