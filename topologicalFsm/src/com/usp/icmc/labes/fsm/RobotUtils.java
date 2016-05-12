package com.usp.icmc.labes.fsm;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.LinkedBlockingQueue;

import org.omg.CORBA.Current;

import com.usp.icmc.ssc5888.CurrentStateUncertainty;
import com.usp.icmc.ssc5888.Maze;
import com.usp.icmc.ssc5888.Robot;
import com.usp.icmc.ssc5888.Robot.Commands;
import com.usp.icmc.ssc5888.TopologicalLocationTree;

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



		for (FsmState s: r.getSyncTree().getStates()) {
			bw.write("		<state id=\""+s.getId()+"\" name=\""+((CurrentStateUncertainty)s).getUncertaintySet().toString()+"\">");bw.write("\n");
			bw.write("			<x>"+(0)+"</x>"); bw.write("\n");
			bw.write("			<y>"+(0)+"</y>"); bw.write("\n");
			if(s.equals(r.getSyncTree().getInitialState())) {
				bw.write("			<initial/>");bw.write("\n");
			}
			bw.write("		</state>");bw.write("\n");

		}

		bw.write("		<!--The list of transitions.-->");bw.write("\n");

		for (FsmTransition tr: r.getSyncTree().getTransitions()) {
			bw.write("		<transition>");bw.write("\n");
			bw.write("			<from>"+tr.getFrom().getId()+"</from>");bw.write("\n");
			bw.write("			<to>"+tr.getTo().getId()+"</to>");bw.write("\n");
			bw.write("			<read>"+tr.getInput()+"</read>");bw.write("\n");
			bw.write("			<transout>"+tr.getOutput()+"</transout>");bw.write("\n");
			bw.write("		</transition>");bw.write("\n");

		}

		bw.write("		<note>");bw.write("\n");
		bw.write("			<text>"+mz.getRobot().getSyncTree().getName()+"</text>");bw.write("\n");
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
		List<FsmTransition> transit = r.getSyncTree().getTransitions();
		for (FsmTransition tr : transit) {
			if(tr.getOutput().equals("deny")) continue;
			pw.println("  "+
					tr.getFrom().getId()
					+" -> "
					+tr.getTo().getId()
					+" [ label =\""+tr.getInput()+"/"+tr.getOutput()+"\"];");
		}

		for (FsmState st : r.getSyncTree().getStates()) {
			pw.println("  "+st.getId()+" [label=\""+((CurrentStateUncertainty)st).getUncertaintySet().toString()+"\"];");
		}
		pw.println("}");
		pw.close();
	}

	public void createSynchronizingTree(Maze mz) {
		createTree(mz, TreeType.SYNCHRONIZING_TREE);
	}
	public void createHomingTree(Maze mz) {
		createTree(mz, TreeType.HOMING_TREE);
	}

	public void createTree(Maze mz, TreeType tt) {
		CurrentStateUncertainty uncert = new CurrentStateUncertainty("0");
		for (FsmState s : mz.getRobot().getTopoMap().getStates())  uncert.getUncertaintySet().add(s);

		mz.getRobot().getSyncTree().addState(uncert);

		createTree(mz.getRobot().getSyncTree(),tt);

		FsmState closestLeaf = depthClosestSingleton(mz.getRobot().getSyncTree());
		FsmState farestLeaf  = depthFarestSingleton(mz.getRobot().getSyncTree());

		mz.getRobot().getSyncTree().setClosestSingleton(getPath(closestLeaf));
		mz.getRobot().getSyncTree().setFarestSingleton(getPath(farestLeaf));
		
		System.out.println(mz.getRobot().getSyncTree().getClosestSingleton());
		System.out.println(mz.getRobot().getSyncTree().getFarestSingleton());
		
		mz.getRobot().getSyncTree().setName("LocationTree='"+tt.name()+"';"+"seed="+mz.getSeed()+"';"+"N="+mz.getN());

	}

	List<FsmTransition> getPath(FsmState state){
		List<FsmTransition> path = new LinkedList<FsmTransition>();
		while (!state.getIn().isEmpty()) {
			((LinkedList) path).push(state.getIn().get(0));
			state = state.getIn().get(0).getFrom();
		}
		return path;
	}

	private FsmState depthClosestSingleton(TopologicalLocationTree tree) {
		int depth = Integer.MAX_VALUE;
		FsmState stateToReturn = null;

		for (FsmState state : tree.getStates()) {
			FsmState temp = state;
			if(((CurrentStateUncertainty)state).getUncertaintySet().size()==1){
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

	private FsmState depthFarestSingleton(TopologicalLocationTree tree) {
		int depth = 0;
		FsmState stateToReturn = null;

		for (FsmState state : tree.getStates()) {
			FsmState temp = state;
			if(((CurrentStateUncertainty)state).getUncertaintySet().size()==1){
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

	private void createTree(TopologicalLocationTree syncTree, TreeType tt) {

		List<Set<FsmState>> aboveLevel = new ArrayList<Set<FsmState>>(); 
		Queue<CurrentStateUncertainty> uncertLst = new LinkedBlockingQueue<CurrentStateUncertainty>();

		uncertLst.add((CurrentStateUncertainty) syncTree.getInitialState());

		int id = 0;
		Map<String, CurrentStateUncertainty> allCurr = new HashMap<String, CurrentStateUncertainty>();
		Set<Set<FsmState>> accessedUncert = new HashSet<Set<FsmState>>();
		Map<String, CurrentStateUncertainty> allSingletonCurr = new HashMap<String, CurrentStateUncertainty>();
		Set<FsmTransition> allTr = new HashSet<FsmTransition>();
		Map<String, CurrentStateUncertainty> io_tr = new HashMap<String, CurrentStateUncertainty>();

		CurrentStateUncertainty stUncert = null;
		CurrentStateUncertainty state = null;
		FsmTransition tr = null;
		String io = null;
		boolean isSingleton,criteria3b;


		while (!uncertLst.isEmpty()) {
			state = uncertLst.remove();
			if(aboveLevel.contains(state.getUncertaintySet())) continue;
			aboveLevel.add(state.getUncertaintySet());

			io_tr.clear();

			for (String in : syncTree.getInputs()) {
				for (FsmState s : state.getUncertaintySet()) {
					tr = getTransition(s, in);
					io = tr.getInput()+tr.getOutput();
					if(!io_tr.containsKey(io)){
						io_tr.putIfAbsent(io,  new CurrentStateUncertainty(Integer.toString(++id)));
					}
					stUncert = io_tr.get(io);
					allCurr.putIfAbsent(stUncert.getId(), stUncert);
					stUncert.getUncertaintySet().add(tr.getTo());
				}
			}

			//			System.out.println(io_tr.toString());
			for (String in : syncTree.getInputs()) {
				for (FsmState s : state.getUncertaintySet()) {
					tr = getTransition(s, in);
					io = tr.getInput()+tr.getOutput();
					stUncert = io_tr.get(io);
					FsmTransition trUncert = new FsmTransition(state, tr.getInput(), tr.getOutput(), stUncert);
					allTr.add(trUncert);
					isSingleton = stUncert.getUncertaintySet().size()==1;
					criteria3b = criteria3bSyncTree(aboveLevel,stUncert);
					if(tt.equals(TreeType.SYNCHRONIZING_TREE)) criteria3b = criteria3bSyncTree(aboveLevel,stUncert);
					else criteria3b = criteria3bHomingTree(aboveLevel,stUncert);
					if(!(isSingleton || criteria3b) && !accessedUncert.contains(stUncert.getUncertaintySet())) {
						uncertLst.add(stUncert);
						accessedUncert.add(stUncert.getUncertaintySet());
					}

					if(isSingleton) allSingletonCurr.putIfAbsent(stUncert.getId(), stUncert);
				}
			}
		}
		//System.out.println(allCurr); System.out.println(allTr);

		for (String curId : allSingletonCurr.keySet()) addState(syncTree,allCurr.get(curId));

		//for (String curId : allCurr.keySet()) syncTree.getStates().add(allCurr.get(curId)); syncTree.getTransitions().addAll(allTr);
	}

	private boolean criteria3bSyncTree(List<Set<FsmState>> aboveLevel, CurrentStateUncertainty stUncert) {
		return aboveLevel.contains(stUncert.getUncertaintySet());
	}

	private boolean criteria3bHomingTree(List<Set<FsmState>> aboveLevel, CurrentStateUncertainty stUncert) {
		for (Set<FsmState> set : aboveLevel) {
			if(stUncert.getUncertaintySet().containsAll(set)) return true;
		}
		return false;
	}

	private void addState(TopologicalLocationTree syncTree, FsmState fsmState) {
		syncTree.getStates().add(fsmState);
		for (FsmTransition tr : fsmState.getIn()) {
			if(!syncTree.getTransitions().contains(tr)) syncTree.getTransitions().add(tr);
			if(!syncTree.getStates().contains(tr.getFrom())) addState(syncTree, tr.getFrom());
		}

	}
}