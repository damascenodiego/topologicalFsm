package com.usp.icmc.ssc5888;

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

import com.usp.icmc.labes.fsm.CurrentStateUncertainty;
import com.usp.icmc.labes.fsm.CurrentStateUncertaintyHomingTree;
import com.usp.icmc.labes.fsm.FsmModel;
import com.usp.icmc.labes.fsm.FsmState;
import com.usp.icmc.labes.fsm.FsmTransition;
import com.usp.icmc.labes.fsm.HomingTree;
import com.usp.icmc.labes.fsm.SynchronizingTree;
import com.usp.icmc.ssc5888.Robot.Commands;

public class RobotUtils {

	private static RobotUtils instance;

	private boolean onlySingletons = true;
	
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


		for (FsmState st: r.getLocationTree().getStates()) {
			bw.write("		<state id=\""+st.getId()+"\" name=\""+st.toString()+"\">");bw.write("\n");
			bw.write("			<x>"+(00)+"</x>"); bw.write("\n");
			bw.write("			<y>"+(00)+"</y>"); bw.write("\n");
			if(st.getId().equals(r.getLocationTree().getInitialState().getId())) bw.write("			<initial/>");bw.write("\n");
			bw.write("		</state>");bw.write("\n");
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
					//+" [ label =\""+tr.getInput()+"/"+tr.getOutput()+"\"];");
					+" [ label =\""+tr.getInput()+"\"];");
		}

		for (FsmState st : r.getLocationTree().getStates()) {
			pw.println("  "+st.getId()+" [label=\""+st.toString()+"\"];");
		}
		pw.println("}");
		pw.close();
	}

	public void createSynchronizingTree(Maze mz) {
		Robot rbt = mz.getRobot();

		CurrentStateUncertainty  uncert = new CurrentStateUncertainty("0");
		for (FsmState s : mz.getRobot().getTopoMap().getStates())  {
			uncert.getUncertaintySet().add(s);
		}

		SynchronizingTree tree = new SynchronizingTree(rbt.getName()+"_SynchronizingTree");
		tree.getInputs().addAll(rbt.getLocationTree().getInputs());
		tree.getOutputs().addAll(rbt.getLocationTree().getOutputs());
		rbt.setLocationTree(tree);

		tree.addState(uncert);
		tree.setInitialState(uncert);


		Set<Set<FsmState>> aboveLevel = new HashSet<Set<FsmState>>();
		Set<Set<FsmState>> nextAboveLevel = new HashSet<Set<FsmState>>();
		Queue<CurrentStateUncertainty> uncertLst = new LinkedBlockingQueue<CurrentStateUncertainty>();
		List<CurrentStateUncertainty> next = new ArrayList<CurrentStateUncertainty>();
		uncertLst.add((CurrentStateUncertainty) tree.getInitialState());

		Map<String, FsmState> 	allStates 		= new HashMap<>();
		Set<FsmTransition> 		allTransitions 	= new HashSet<>();
		allStates.put(uncert.getId(),uncert);

		int id = 0;
		Map<String, CurrentStateUncertainty> allSingletonCurr = new HashMap<String, CurrentStateUncertainty>();
		Map<String, CurrentStateUncertainty> io_tr = new HashMap<String, CurrentStateUncertainty>();

		CurrentStateUncertainty stUncert = null;
		CurrentStateUncertainty state = null;
		FsmTransition tr = null;
		boolean criteria3a,criteria3b;


		while (!uncertLst.isEmpty()) {
			state = ((CurrentStateUncertainty)uncertLst.remove());

			nextAboveLevel.add(state.getUncertaintySet());

			io_tr.clear();

			for (String in : tree.getInputs()) {
				if(!io_tr.containsKey(in)){
					io_tr.putIfAbsent(in,  new CurrentStateUncertainty(Integer.toString(++id)));
				}
				stUncert = io_tr.get(in);
				allStates.put(stUncert.getId(), stUncert);
				for (FsmState s : state.getUncertaintySet()) {
					tr = getTransition(s, in);
					stUncert.getUncertaintySet().add(tr.getTo());
				}
				FsmTransition trUncert = new FsmTransition(state, in, "", stUncert);
				allTransitions.add(trUncert);
				criteria3a = criteria3a(stUncert);
				criteria3b = criteria3bSyncTree(aboveLevel,stUncert);
				if(!(criteria3a || criteria3b)) {
					next.add(stUncert);
				}

				if(criteria3a(trUncert.getTo())) {
					allSingletonCurr.putIfAbsent(stUncert.getId(), stUncert);
				}
			}
			if(uncertLst.isEmpty()) {
				aboveLevel.addAll(nextAboveLevel);
				uncertLst.addAll(next);
				next.clear();
				nextAboveLevel.clear();
			}
		}
		if(onlySingletons){
			Set<FsmTransition> allSingletonTr = new HashSet<FsmTransition>();
			Map<String,FsmState> allSingletonStates = new HashMap<>();
			for (String curId : allSingletonCurr.keySet()) {
				FsmState s = allSingletonCurr.get(curId);
				allSingletonStates.putIfAbsent(s.getId(),s);
				while (s.getIn().size()!=0) {
					FsmTransition trSingl = s.getIn().get(0);
					allSingletonTr.add(trSingl);
					s = trSingl.getFrom();
					s.getOut().clear();
					s.getOut().add(trSingl);
					allSingletonStates.putIfAbsent(s.getId(),s);
				}
			}
			tree.getTransitions().addAll(allSingletonTr);
			tree.getStates().clear();
			tree.getStates().addAll(allSingletonStates.values());
		}else{
			tree.getTransitions().addAll(allTransitions);
			tree.getStates().clear();
			tree.getStates().addAll(allStates.values());
		}

		tree.setClosestSingleton(getPath(depthClosestSingleton(tree)));
		tree.setFarestSingleton(getPath(depthFarestSingleton(tree)));

		tree.setClosestAllSingleton(getPath(depthClosestAllSingleton(tree)));
		tree.setFarestAllSingleton(getPath(depthFarestAllSingleton(tree)));

		//		System.out.println(mz.getRobot().getLocationTree().getClosestSingleton());
		//		System.out.println(mz.getRobot().getLocationTree().getFarestSingleton());

		mz.getRobot().getLocationTree().setName(tree.getClass().getSimpleName()+";"+"seed="+mz.getSeed()+"';"+"N="+mz.getN());

	}

	public void createHomingTree(Maze mz) {
		Robot rbt = mz.getRobot();

		CurrentStateUncertaintyHomingTree uncert = new CurrentStateUncertaintyHomingTree("0");
		for (FsmState s : mz.getRobot().getTopoMap().getStates())  {
			uncert.getUncertaintyMap().putIfAbsent("",new HashSet<FsmState>());
			uncert.getUncertaintyMap().get("").add(s);
			uncert.getUncertaintySet().add(s);
		}

		HomingTree tree = new HomingTree(rbt.getName()+"_HomingTree");
		tree.getInputs().addAll(rbt.getLocationTree().getInputs());
		tree.getOutputs().addAll(rbt.getLocationTree().getOutputs());
		rbt.setLocationTree(tree);

		tree.addState(uncert);
		tree.setInitialState(uncert);


		Set<Set<FsmState>> aboveLevel = new HashSet<Set<FsmState>>();
		Set<Set<FsmState>> nextAboveLevel = new HashSet<Set<FsmState>>();
		Queue<CurrentStateUncertaintyHomingTree> uncertLst = new LinkedBlockingQueue<>();
		List<CurrentStateUncertaintyHomingTree> next = new ArrayList<>();
		uncertLst.add((CurrentStateUncertaintyHomingTree) tree.getInitialState());

		Map<String, FsmState> 	allStates 		= new HashMap<>();
		Set<FsmTransition> 		allTransitions 	= new HashSet<>();
		allStates.put(uncert.getId(),uncert);
		
		int id = 0;
		Map<String, CurrentStateUncertaintyHomingTree> allSingletonCurr = new HashMap<>();
		Map<String, CurrentStateUncertaintyHomingTree> io_tr = new HashMap<>();

		CurrentStateUncertaintyHomingTree stUncert = null;
		CurrentStateUncertaintyHomingTree state = null;
		FsmTransition tr = null;
		String output = null;
		boolean criteria3a,criteria3b;


		while (!uncertLst.isEmpty()) {
			state = ((CurrentStateUncertaintyHomingTree)uncertLst.remove());

			nextAboveLevel.add(state.getUncertaintySet());

			io_tr.clear();

			for (String in : tree.getInputs()) {
				if(!io_tr.containsKey(in)){
					io_tr.putIfAbsent(in,  new CurrentStateUncertaintyHomingTree(Integer.toString(++id)));
				}
				stUncert = io_tr.get(in);
				allStates.put(stUncert.getId(), stUncert);
				for(String key : state.getUncertaintyMap().keySet()){
					for (FsmState s : state.getUncertaintyMap().get(key)) {
						tr = getTransition(s, in);
						output = tr.getOutput();
						String kout = (key.length()==0)?output:key+","+output;
						stUncert.getUncertaintyMap().putIfAbsent(kout,new HashSet<FsmState>());
						stUncert.getUncertaintyMap().get(kout).add(tr.getTo());
						stUncert.getUncertaintySet().add(tr.getTo());
					}
				}
				FsmTransition trUncert = new FsmTransition(state, in, "", stUncert);
				allTransitions.add(trUncert);
				criteria3a = criteria3a(stUncert);
				criteria3b = criteria3bHomingTree(aboveLevel,stUncert);
				if(!(criteria3a || criteria3b)) {
					next.add(stUncert);
				}

				if(criteria3a(trUncert.getTo())) {
					allSingletonCurr.putIfAbsent(stUncert.getId(), stUncert);
				}
			}
			if(uncertLst.isEmpty()) {
				aboveLevel.addAll(nextAboveLevel);
				uncertLst.addAll(next);
				next.clear();
				nextAboveLevel.clear();
			}
		}
		
		if(onlySingletons){
			Set<FsmTransition> allSingletonTr = new HashSet<FsmTransition>();
			Map<String,FsmState> allSingletonStates = new HashMap<>();
			for (String curId : allSingletonCurr.keySet()) {
				FsmState s = allSingletonCurr.get(curId);
				allSingletonStates.putIfAbsent(s.getId(),s);
				while (s.getIn().size()!=0) {
					FsmTransition trSingl = s.getIn().get(0);
					allSingletonTr.add(trSingl);
					s = trSingl.getFrom();
					s.getOut().clear();
					s.getOut().add(trSingl);
					allSingletonStates.putIfAbsent(s.getId(),s);
				}
			}
			tree.getTransitions().addAll(allSingletonTr);
			tree.getStates().clear();
			tree.getStates().addAll(allSingletonStates.values());
		}else{
			tree.getTransitions().addAll(allTransitions);
			tree.getStates().clear();
			tree.getStates().addAll(allStates.values());
		}

		tree.setClosestSingleton(getPath(depthClosestSingleton(tree)));
		tree.setFarestSingleton(getPath(depthFarestSingleton(tree)));

		tree.setClosestAllSingleton(getPath(depthClosestAllSingleton(tree)));
		tree.setFarestAllSingleton(getPath(depthFarestAllSingleton(tree)));

		//		System.out.println(mz.getRobot().getLocationTree().getClosestSingleton());
		//		System.out.println(mz.getRobot().getLocationTree().getFarestSingleton());

		mz.getRobot().getLocationTree().setName(tree.getClass().getSimpleName()+";"+"seed="+mz.getSeed()+"';"+"N="+mz.getN());

	}

	public List<FsmTransition> getPath(FsmState state){
		List<FsmTransition> path = new LinkedList<FsmTransition>();
		while (!state.getIn().isEmpty()) {
			((LinkedList<FsmTransition>) path).push(state.getIn().get(0));
			state = state.getIn().get(0).getFrom();
		}
		return path;
	}

	private FsmState depthClosestAllSingleton(FsmModel fsmModel) {
		int depth = Integer.MAX_VALUE;
		FsmState stateToReturn = null;

		for (FsmState state : fsmModel.getStates()) {
			FsmState temp = state;
			if(criteria3a(temp)){
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

	private FsmState depthFarestAllSingleton(FsmModel tree) {
		int depth = 0;
		FsmState stateToReturn = null;
	
		for (FsmState state : tree.getStates()) {
			FsmState temp = state;
			if(criteria3a(temp)){
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
	
	private FsmState depthClosestSingleton(FsmModel fsmModel) {
		int depth = Integer.MAX_VALUE;
		FsmState stateToReturn = null;

		for (FsmState state : fsmModel.getStates()) {
			FsmState temp = state;
			if(anySingleton(temp)){
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
			if(anySingleton(temp)){
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

	private boolean criteria3a(FsmState state) {
		if(state instanceof CurrentStateUncertaintyHomingTree){
			CurrentStateUncertaintyHomingTree stUncert  = (CurrentStateUncertaintyHomingTree) state;
			return criteria3a(stUncert);
		}else if(state instanceof CurrentStateUncertainty){
			CurrentStateUncertainty stUncert  = (CurrentStateUncertainty) state;
			return criteria3a(stUncert);
		}
		return false;
	
	}

	private boolean criteria3a(CurrentStateUncertaintyHomingTree stUncert) {
		for (String k : stUncert.getUncertaintyMap().keySet()) {
			if(stUncert.getUncertaintyMap().get(k).size()!=1) return false;
		}
		return true;
	}

	private boolean criteria3a(CurrentStateUncertainty stUncert) {
		if(stUncert.getUncertaintySet().size()!=1) return false;
		return true;
	}

	private boolean anySingleton(FsmState state) {
		if(state instanceof CurrentStateUncertaintyHomingTree){
			CurrentStateUncertaintyHomingTree stUncert  = (CurrentStateUncertaintyHomingTree) state;
			for (String k : stUncert.getUncertaintyMap().keySet()) {
				if(stUncert.getUncertaintyMap().get(k).size()==1) return true;
			}
		}else if(state instanceof CurrentStateUncertainty){
			CurrentStateUncertainty stUncert  = (CurrentStateUncertainty) state;
			if(stUncert.getUncertaintySet().size()==1) return true;
		}
		return false;
	}

	private boolean criteria3bHomingTree(Set<Set<FsmState>> aboveLevel, CurrentStateUncertaintyHomingTree stUncert) {
		for (Set<FsmState> csuh : aboveLevel) {
			if(csuh.equals(stUncert.getUncertaintySet())) return true;
		}
		return false;
	}


	private boolean criteria3bSyncTree(Set<Set<FsmState>> aboveLevel, CurrentStateUncertainty stUncert) {
		for (Set<FsmState> csuh : aboveLevel) {
			if(csuh.equals(stUncert.getUncertaintySet())) return true;
		}
		return false;
	}
	
	public void setOnlySingletons(boolean onlySingletons) {
		this.onlySingletons = onlySingletons;
	}

	public boolean isOnlySingletons() {
		return onlySingletons;
	}
	
}