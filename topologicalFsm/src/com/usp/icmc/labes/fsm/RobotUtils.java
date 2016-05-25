package com.usp.icmc.labes.fsm;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.LinkedBlockingQueue;

import com.usp.icmc.ssc5888.CurrentStateUncertainty;
import com.usp.icmc.ssc5888.Maze;
import com.usp.icmc.ssc5888.Robot;
import com.usp.icmc.ssc5888.Robot.Commands;
import com.usp.icmc.ssc5888.TopologicalLocationTree;
import com.usp.icmc.ssc5888.TopologicalMap;

public class RobotUtils {

	private static RobotUtils instance;

	private RobotUtils(){ }

	public static RobotUtils getInstance() {
		if(instance==null) instance = new RobotUtils();
		return instance;
	}

	public FsmState updateTopologicalMap(Maze maze, int i, int j, FsmState curr) {

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

	public FsmTransition getTransition(FsmState st, Commands cmd){
		for (FsmTransition tr: st.getOut()) {
			if(tr.getInput().equals(cmd.toString())){
				return tr;
			}
		}
		return null;
	}

	public FsmTransition getTransition(FsmState st, String cmd){
		for (FsmTransition tr: st.getOut()) {
			if(tr.getInput().equals(cmd)){
				return tr;
			}
		}
		return null;
	}

	public void saveTopoMap(Maze mz, File f) throws IOException{
		Robot r = mz.getRobot();

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
			if(s.equals(r.getTopoMap().getInitialState())) {
				bw.write("			<initial/>");bw.write("\n");
			}
			bw.write("		</state>");bw.write("\n");

		}

		bw.write("		<!--The list of transitions.-->");bw.write("\n");

		for (FsmTransition tr: r.getTopoMap().getTransitions()) {
			//			if(tr.getFrom().getId().equals(tr.getTo().getId())) continue;
			bw.write("		<transition>");bw.write("\n");
			bw.write("			<from>"+tr.getFrom().hashCode()+"</from>");bw.write("\n");
			bw.write("			<to>"+tr.getTo().hashCode()+"</to>");bw.write("\n");
			bw.write("			<read>"+tr.getInput()+"</read>");bw.write("\n");
			bw.write("			<transout>"+tr.getOutput()+"</transout>");bw.write("\n");
			bw.write("		</transition>");bw.write("\n");

		}

		bw.write("		<note>");bw.write("\n");
		bw.write("			<text>"+"seed="+mz.getSeed()+"';"+"N="+mz.getN()+"</text>");bw.write("\n");
		bw.write("			<x>"+(0)+"</x>"); bw.write("\n");
		bw.write("			<y>"+(0)+"</y>"); bw.write("\n");
		bw.write("		</note>");bw.write("\n");

		bw.write("	</automaton>");bw.write("\n");
		bw.write("</structure>");

		bw.close();

	}

	public void saveLocationTree(Maze mz, File f) throws IOException{
		Robot r = mz.getRobot();
		BufferedWriter bw = new BufferedWriter(new FileWriter(f));

		bw.write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?><!--Created with JFLAP 6.4.--><structure>"); bw.write("\n");
		bw.write("	<type>mealy</type>");bw.write("\n");
		bw.write("	<automaton>");bw.write("\n");
		bw.write("		<!--The list of states.-->");bw.write("\n");


		Queue<FsmState> queue = new LinkedBlockingQueue<FsmState>();

		queue.add(r.getLocationTree().getInitialState());
		
		int y = 1;

		bw.write("		<state id=\""+r.getLocationTree().getInitialState().getId()+"\" name=\""+r.getLocationTree().getInitialState().toString()+"\">");bw.write("\n");
		bw.write("			<x>"+0+"</x>"); bw.write("\n");
		bw.write("			<y>"+0+"</y>"); bw.write("\n");
		bw.write("			<initial/>");bw.write("\n");
		bw.write("		</state>");bw.write("\n");

		
		while (!queue.isEmpty()) {
			FsmState s = queue.remove();
			int x = 1;

			for (FsmTransition tr : s.getOut()) {
				x++;
				queue.add(tr.getTo());
				
				bw.write("		<state id=\""+tr.getTo().getId()+"\" name=\""+tr.getTo().toString()+"\">");bw.write("\n");
				bw.write("			<x>"+(x*100)+"</x>"); bw.write("\n");
				bw.write("			<y>"+(y*100)+"</y>"); bw.write("\n");
				bw.write("		</state>");bw.write("\n");
			}
			y++;
		}


		bw.write("		<!--The list of transitions.-->");bw.write("\n");

		for (FsmTransition tr: r.getLocationTree().getTransitions()) {
			bw.write("		<transition>");bw.write("\n");
			bw.write("			<from>"+tr.getFrom().getId()+"</from>");bw.write("\n");
			bw.write("			<to>"+tr.getTo().getId()+"</to>");bw.write("\n");
			bw.write("			<read>"+tr.getInput()+"</read>");bw.write("\n");
			bw.write("			<transout>"+tr.getOutput()+"</transout>");bw.write("\n");
			bw.write("		</transition>");bw.write("\n");

		}

		bw.write("		<note>");bw.write("\n");
		bw.write("			<text>"+mz.getRobot().getLocationTree().getName()+"</text>");bw.write("\n");
		bw.write("			<x>"+(0)+"</x>"); bw.write("\n");
		bw.write("			<y>"+(0)+"</y>"); bw.write("\n");
		bw.write("		</note>");bw.write("\n");
		bw.write("	</automaton>");bw.write("\n");
		bw.write("</structure>");

		bw.close();

	}


	public void saveLocationTreeAsDot(Maze mz, File f) throws IOException{
		Robot r = mz.getRobot();
		PrintWriter pw = new PrintWriter(f);

		pw.println("digraph rbac2Fsm {");
		List<FsmTransition> transit = r.getLocationTree().getTransitions();
		for (FsmTransition tr : transit) {
			if(tr.getOutput().equals("deny")) continue;
			pw.println("  "+
					tr.getFrom().getId()
					+" -> "
					+tr.getTo().getId()
					+" [ label =\""+tr.getInput()+"/"+tr.getOutput()+"\"];");
		}

		for (FsmState st : r.getLocationTree().getStates()) {
			pw.println("  "+st.getId()+" [label=\""+st.toString()+"\"];");
		}
		pw.println("}");
		pw.close();
	}

	//	public void createSynchronizingTree(Maze mz) {
	//
	//		CurrentStateUncertainty uncert = new CurrentStateUncertainty("0");
	//		for (FsmState s : mz.getRobot().getTopoMap().getStates())  uncert.getUncertaintySet().add(s);
	//
	//		mz.getRobot().getLocationTree().addState(uncert);
	//
	//		createTree(mz.getRobot().getLocationTree(),tt);
	//
	//		FsmState closestLeaf = depthClosestSingleton(mz.getRobot().getLocationTree());
	//		FsmState farestLeaf  = depthFarestSingleton(mz.getRobot().getLocationTree());
	//
	//		mz.getRobot().getLocationTree().setClosestSingleton(getPath(closestLeaf));
	//		mz.getRobot().getLocationTree().setFarestSingleton(getPath(farestLeaf));
	//
	//		//		System.out.println(mz.getRobot().getLocationTree().getClosestSingleton());
	//		//		System.out.println(mz.getRobot().getLocationTree().getFarestSingleton());
	//
	//		mz.getRobot().getLocationTree().setName("LocationTree='"+tt.name()+"';"+"seed="+mz.getSeed()+"';"+"N="+mz.getN());
	//
	//	}

	public void createHomingTree(Maze mz) {
		Robot rbt = mz.getRobot();

		CurrentStateUncertaintyHomingTree uncert = new CurrentStateUncertaintyHomingTree("0");
		for (FsmState s : mz.getRobot().getTopoMap().getStates())  {
			uncert.getUncertaintyMap().putIfAbsent("EMPTY",new HashSet<FsmState>());
			uncert.getUncertaintyMap().get("EMPTY").add(s);
			uncert.getUncertaintySet().add(s);
		}

		HomingTree homingTree = new HomingTree(rbt.getName()+"_HomingTree");
		homingTree.getInputs().addAll(rbt.getLocationTree().getInputs());
		homingTree.getOutputs().addAll(rbt.getLocationTree().getOutputs());
		rbt.setLocationTree(homingTree);

		homingTree.addState(uncert);
		homingTree.setInitialState(uncert);


		Set<Set<FsmState>> aboveLevel = new HashSet<Set<FsmState>>(); 
		Queue<CurrentStateUncertaintyHomingTree> uncertLst = new LinkedBlockingQueue<CurrentStateUncertaintyHomingTree>();
		uncertLst.add((CurrentStateUncertaintyHomingTree) homingTree.getInitialState());

		int id = 0;
		Map<String, FsmState> allCurr = new HashMap<String, FsmState>();
		Set<FsmTransition> allTr = new HashSet<FsmTransition>();
		//Set<Set<FsmState>> accessedUncert = new HashSet<Set<FsmState>>();
		Map<String, CurrentStateUncertaintyHomingTree> allSingletonCurr = new HashMap<String, CurrentStateUncertaintyHomingTree>();
		Map<String, CurrentStateUncertaintyHomingTree> io_tr = new HashMap<String, CurrentStateUncertaintyHomingTree>();

		CurrentStateUncertaintyHomingTree stUncert = null;
		CurrentStateUncertaintyHomingTree state = null;
		FsmTransition tr = null;
		String output = null;
		boolean criteria3a,criteria3b;


		while (!uncertLst.isEmpty()) {
			state = ((CurrentStateUncertaintyHomingTree)uncertLst.remove());

			if(aboveLevel.contains(state.getUncertaintySet())) continue;
			aboveLevel.add(state.getUncertaintySet());

			io_tr.clear();

			for (String in : homingTree.getInputs()) {
				if(!io_tr.containsKey(in)){
					io_tr.putIfAbsent(in,  new CurrentStateUncertaintyHomingTree(Integer.toString(++id)));
				}
				stUncert = io_tr.get(in);
				for(String key : state.getUncertaintyMap().keySet()){
					for (FsmState s : state.getUncertaintyMap().get(key)) {
						tr = getTransition(s, in);
						output = tr.getOutput();
						String kout = key+","+output;
						stUncert.getUncertaintyMap().putIfAbsent(kout,new HashSet<FsmState>());
						stUncert.getUncertaintyMap().get(kout).add(tr.getTo());
						stUncert.getUncertaintySet().add(tr.getTo());
					}
				}
				FsmTransition trUncert = new FsmTransition(state, in, "", stUncert);
				criteria3a = criteria3aHomingTree(stUncert);
				criteria3b = criteria3bHomingTree(aboveLevel,stUncert);
				if(!(criteria3a || criteria3b)) {
					uncertLst.add(stUncert);
				}
				
				allCurr.putIfAbsent(stUncert.getId(), stUncert);
				allTr.add(trUncert);
				if(criteria3a) {
					allSingletonCurr.putIfAbsent(stUncert.getId(), stUncert);
				}
			}
		}
		//System.out.println(allCurr); System.out.println(allTr);

		//		for (String curId : allSingletonCurr.keySet()) addState(syncTree,allCurr.get(curId));
		//
		for (String curId : allCurr.keySet()) homingTree.getStates().add(allCurr.get(curId)); 
		homingTree.getTransitions().addAll(allTr);
		//
				FsmState closestLeaf = depthClosestSingleton(mz.getRobot().getLocationTree());
				FsmState farestLeaf  = depthFarestSingleton(mz.getRobot().getLocationTree());
		//
//				homingTree.setClosestSingleton(getPath(closestLeaf));
//				homingTree.setFarestSingleton(getPath(farestLeaf));

		//		System.out.println(mz.getRobot().getLocationTree().getClosestSingleton());
		//		System.out.println(mz.getRobot().getLocationTree().getFarestSingleton());

		mz.getRobot().getLocationTree().setName("HomingTree;"+"seed="+mz.getSeed()+"';"+"N="+mz.getN());

	}

	public List<FsmTransition> getPath(FsmState state){
		List<FsmTransition> path = new LinkedList<FsmTransition>();
		while (!state.getIn().isEmpty()) {
			((LinkedList<FsmTransition>) path).push(state.getIn().get(0));
			state = state.getIn().get(0).getFrom();
		}
		return path;
	}

	private FsmState depthClosestSingleton(FsmModel fsmModel) {
		int depth = Integer.MAX_VALUE;
		FsmState stateToReturn = null;

		for (FsmState state : fsmModel.getStates()) {
			FsmState temp = state;
			if(anySingleton((CurrentStateUncertaintyHomingTree) state)){
				int counter = 0;
				while (!state.getIn().isEmpty()) {
					state = state.getIn().get(0).getFrom();
					counter++;
				}
				if(depth>counter) {
					depth = counter;
					stateToReturn = temp;
				}
			}
		}
		return stateToReturn;
	}

	private FsmState depthFarestSingleton(FsmModel tree) {
		int depth = 0;
		FsmState stateToReturn = null;

		for (FsmState state : tree.getStates()) {
			FsmState temp = state;
			if(anySingleton((CurrentStateUncertaintyHomingTree) state)){
				int counter = 0;
				while (!state.getIn().isEmpty()) {
					state = state.getIn().get(0).getFrom();
					counter++;
				}
				if(depth<counter) {
					depth = counter;
					stateToReturn = temp;
				}
			}
		}
		return stateToReturn;
	}

	private enum TreeType {
		SYNCHRONIZING_TREE,
		HOMING_TREE
	};

	private boolean anySingleton(CurrentStateUncertaintyHomingTree stUncert) {
		for (String k : stUncert.getUncertaintyMap().keySet()) {
			if(stUncert.getUncertaintyMap().get(k).size()==1) return true;
		}
		return false;
	}

	private boolean criteria3aHomingTree(CurrentStateUncertaintyHomingTree stUncert) {
		for (String k : stUncert.getUncertaintyMap().keySet()) {
			if(stUncert.getUncertaintyMap().get(k).size()!=1) return false;
		}
		return true;
	}

	private boolean criteria3bHomingTree(Set<Set<FsmState>> aboveLevel, CurrentStateUncertaintyHomingTree stUncert) {
		for (Set<FsmState> csuh : aboveLevel) {
			if(csuh.equals(stUncert.getUncertaintySet())) return true;
		}
		return false;
	}

}