import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionGroup;
import org.apache.commons.cli.Options;

import com.usp.icmc.labes.fsm.CurrentStateUncertainty;
import com.usp.icmc.labes.fsm.CurrentStateUncertaintyHomingTree;
import com.usp.icmc.labes.fsm.DrawUtils;
import com.usp.icmc.labes.fsm.FsmState;
import com.usp.icmc.labes.fsm.FsmTransition;
import com.usp.icmc.labes.fsm.ICurrentStateUncertainty;
import com.usp.icmc.labes.fsm.ICurrentStateUncertaintyTree;
import com.usp.icmc.ssc5888.Maze;
import com.usp.icmc.ssc5888.MazeUtils;
import com.usp.icmc.ssc5888.RobotUtils;

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
	private static final String 	CLOSE_PARAMETER			= "close";
	private static final String 	HOMING_PARAMETER 		= "homing";
	private static final String 	SYNCHRONIZING_PARAMETER = "synchronizing";
	private static final String 	HELP_PARAMETER 			= "help";
	private static final String 	HELP_PARAMETER_SHORT 	= "h";
	private static final String 	COMPLETE_TREE_PARAMETER = "complete";
	private static final String 	SEED_PATH_PARAMETER 	= "seedp";



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

			if(cmd.hasOption(HELP_PARAMETER) || cmd.hasOption(HELP_PARAMETER_SHORT)){
				HelpFormatter formatter = new HelpFormatter();
				formatter.printHelp("topologicalFsm", options);
				System.exit(0);
			}
			
			if(cmd.hasOption(COMPLETE_TREE_PARAMETER)) {
				RobotUtils.getInstance().setOnlySingletons(false);
			}
			if(cmd.hasOption(N_PARAMETER)) {
				N = Integer.valueOf(cmd.getOptionValue(N_PARAMETER));
			}
			if(cmd.hasOption(POS_PARAMETER)) {
				String coords[] = cmd.getOptionValue(POS_PARAMETER).split(",");
				x = Integer.valueOf(coords[0]); 
				y = Integer.valueOf(coords[1]);
				if((x<=0 || x>N) || (y<=0 || y>N)) {
					throw new Exception("Position ("+x+","+y+") out of bounds");
				}
			}else{
				x = StdRandom.uniform(N)+1; 
				y = StdRandom.uniform(N)+1;
			}

			if(cmd.hasOption(SEED_PARAMETER)) {
				SEED = Long.valueOf(cmd.getOptionValue(SEED_PARAMETER));
				StdRandom.setSeed(SEED);
			}

			if(!cmd.hasOption(SHOW_WINDOW_PARAMETER)) {
				DrawUtils.getInstance().setShowWindow(false);
			}
			System.out.println("Seed:\t"+StdRandom.getSeed());
			System.out.println("RobotPosition:\t("+x+","+y+")");
			System.out.println("N:\t"+N);
			maze = new Maze(N,x,y);
			maze.solve();


			Date di = new Date();
			if(cmd.hasOption(HOMING_PARAMETER)){
				RobotUtils.getInstance().createHomingTree(maze);
			}else {
				RobotUtils.getInstance().createSynchronizingTree(maze);
			}
			Date df = new Date();
			ICurrentStateUncertaintyTree ht = (ICurrentStateUncertaintyTree) maze.getRobot().getLocationTree();

			System.out.println("ClosestSingleton:\t"+ht.getClosestSingleton().size());
			System.out.println("FarestSingleton:\t"+ht.getFarestSingleton().size());
			System.out.println("ClosestAllSingleton:\t"+ht.getClosestAllSingleton().size());
			System.out.println("FarestAllSingleton:\t"+ht.getFarestAllSingleton().size());
			System.out.println("Time:\t"+(df.getTime()-di.getTime()));

			if(cmd.hasOption(SAVE_PARAMETER)){
				String tStamp = new SimpleDateFormat("yyyyMMddhhmm").format(new Date());
				File folder = new File(new File("topologicalFsm"),"topoMap_"+"_N_"+N+"/");
				folder.mkdirs();
				String fname = "topoMap"+"_N_"+N+"_"+tStamp+"_SEED_"+SEED;
				if(DrawUtils.getInstance().getShowWindow()) {
					StdDraw.show(0);
					maze.draw();
					maze.fillCurrent();
					StdDraw.show(1000);
					StdDraw.save(new File(folder,fname+".png").getAbsolutePath());
				}
				RobotUtils.getInstance().saveTopoMap(maze, new File(folder,fname+".jff"));
				RobotUtils.getInstance().saveLocationTree(maze, new File(folder,fname+"_"+ht.getClass().getSimpleName()+".jff"));
				RobotUtils.getInstance().saveLocationTreeAsDot(maze, new File(folder,fname+"_"+ht.getClass().getSimpleName()+".dot"));
				MazeUtils.getInstance().saveMaze(new File(folder,fname+"_"+ht.getClass().getSimpleName()+".topomap.txt"), maze);
				BufferedWriter bw = new BufferedWriter(new FileWriter(new File(folder,fname+"_"+ht.getClass().getSimpleName()+".singletons.txt")));

				bw.write("ClosestSingleton:\n");
				bw.write( "\t" +
						(ht.getClosestSingleton().get(0).getFrom()) +
						"\n"
						);
				for (FsmTransition tr : ht.getClosestSingleton()) {
					bw.write( "\t" +
							//							(tr.getFrom()) + " -- "+
							tr.getInput() +
							//							" / " + tr.getOutput()+
							" -> " +
							(tr.getTo()) +
							"\n"
							);
				}

				bw.write("FarestSingleton:\n");
				bw.write( "\t" +
						(ht.getFarestSingleton().get(0).getFrom()) +
						"\n"
						);
				for (FsmTransition tr : ht.getFarestSingleton()) {
					bw.write( "\t" +
							//							(tr.getFrom()) + " -- "+
							tr.getInput() +
							//							" / " + tr.getOutput()+
							" -> " +
							(tr.getTo()) +
							"\n"
							);
				}

				bw.write("ClosestAllSingleton:\n");
				bw.write( "\t" +
						(ht.getClosestAllSingleton().get(0).getFrom()) +
						"\n"
						);
				for (FsmTransition tr : ht.getClosestAllSingleton()) {
					bw.write( "\t" +
							//							(tr.getFrom()) + " -- "+
							tr.getInput() +
							//							" / " + tr.getOutput()+
							" -> " +
							(tr.getTo()) +
							"\n"
							);
				}

				bw.write("FarestAllSingleton:\n");
				bw.write( "\t" +
						(ht.getFarestAllSingleton().get(0).getFrom()) +
						"\n"
						);
				for (FsmTransition tr : ht.getFarestAllSingleton()) {
					bw.write( "\t" +
							//							(tr.getFrom()) + " -- "+
							tr.getInput() +
							//							" / " + tr.getOutput()+
							" -> " +
							(tr.getTo()) +
							"\n"
							);
				}

				bw.close();

				if(DrawUtils.getInstance().getShowWindow()) {
					int count = 0;
					int stepNo = 1;
					StringBuilder sb = new StringBuilder();
					FsmState csu = maze.getRobot().getLocationTree().getInitialState();
					FsmTransition tr = null;
					boolean singletonFound = false;
					if(cmd.hasOption(SEED_PATH_PARAMETER)){
						SEED = Long.valueOf(cmd.getOptionValue(SEED_PATH_PARAMETER));
						StdRandom.setSeed(SEED);
					}else{
						SEED = StdRandom.getSeed();
					}
					System.out.println("SeedPath:\t"+SEED);
					do {
						tr = csu.getOut().get(StdRandom.uniform(csu.getOut().size()));

						// save map (from)
						csu =  tr.getFrom();
						StdDraw.clear();
						maze.draw();
						if(csu instanceof CurrentStateUncertaintyHomingTree) {
							maze.fillCurrent((CurrentStateUncertaintyHomingTree) csu,sb.toString());
							maze.writeText(((maze.getN()+2)/2.0), 0.5, Integer.toString(stepNo)+") "+tr.getInput());
						}
						else if(csu instanceof CurrentStateUncertainty) {
							maze.fillCurrent((CurrentStateUncertainty) csu);
							maze.writeText(((maze.getN()+2)/2.0), 0.5, Integer.toString(stepNo)+") "+tr.getInput());
						}
						maze.fillCurrent();
						StdDraw.show(1000);
						StdDraw.save(new File(folder,fname+"_"+ht.getClass().getSimpleName()+"_step_"+(++count)+".png").getAbsolutePath());

						boolean out = maze.runCommand(tr.getInput());
						if(sb.length()==0) {
							sb.append(Boolean.toString(out));
						}else{
							sb.append(",");
							sb.append(Boolean.toString(out));
						}

						// save map (to)
						csu =  tr.getTo();
						StdDraw.clear();
						maze.draw();
						if(csu instanceof CurrentStateUncertaintyHomingTree) {
							maze.fillCurrent((CurrentStateUncertaintyHomingTree) csu,sb.toString());
							maze.writeText(((maze.getN()+2)/2.0), 0.5, Integer.toString(stepNo)+") "+tr.getInput()+"/"+out);
							singletonFound = ((CurrentStateUncertaintyHomingTree) csu).getUncertaintyMap().get(sb.toString()).size()==1;
						}
						else if(csu instanceof CurrentStateUncertainty) {
							maze.fillCurrent((CurrentStateUncertainty) csu);
							maze.writeText(((maze.getN()+2)/2.0), 0.5, Integer.toString(stepNo)+") "+tr.getInput());
							singletonFound = ((CurrentStateUncertainty) csu).getUncertaintySet().size()==1;
						}
						maze.fillCurrent();
						StdDraw.show(1000);
						StdDraw.save(new File(folder,fname+"_"+ht.getClass().getSimpleName()+"_step_"+(++count)+".png").getAbsolutePath());
						++stepNo;
					}while (!tr.getTo().getOut().isEmpty() && !singletonFound);
				}
			}
			if(cmd.hasOption(CLOSE_PARAMETER)) System.exit(0);
		} catch (Exception e1) {
			e1.printStackTrace();
			System.exit(1);
		}
	}

	private static void setupCliOptions() {
		// create Options object
		options = new Options();


		Option nOption		 	= new Option(N_PARAMETER, 			true,	"size of the map. The map is squared.");
		Option seedOption 		= new Option(SEED_PARAMETER, 		true, 	"Custom seed for generating map. Random seed set as default.");
		Option seedPathOption 	= new Option(SEED_PATH_PARAMETER, 	true, 	"Custom seed for path generation. Random seed set as default.");
		Option posOption 		= new Option(POS_PARAMETER, 		true, 	"Robot custom position. Random position set as default.");
		Option saveDataOption 	= new Option(SAVE_PARAMETER, 		false, 	"Saves data generated.");
		Option showWindowOption = new Option(SHOW_WINDOW_PARAMETER, false, 	"Show window.");
		Option windowCloseOption= new Option(CLOSE_PARAMETER, false, 	"Close window.");
		Option onlySingletons 	= new Option(COMPLETE_TREE_PARAMETER, 	false, 	"Generates complete synchronizing/homing trees. Only singleton nodes by default.");
		Option helpMenu 		= new Option(HELP_PARAMETER_SHORT,HELP_PARAMETER, false, 	"Help menu.");
		
		OptionGroup og = new OptionGroup();

		og.addOption(new Option(HOMING_PARAMETER, "Generate homing tree. Synchronizing tree by default"));
		og.addOption(new Option(SYNCHRONIZING_PARAMETER, "Generate synchronizing tree"));

		options.addOption(nOption);
		options.addOption(onlySingletons);
		options.addOption(seedOption);
		options.addOption(posOption);
		options.addOption(saveDataOption);
		options.addOption(showWindowOption);
		options.addOption(windowCloseOption);
		options.addOptionGroup(og);
		options.addOption(helpMenu);
		options.addOption(seedPathOption);

	}

}
