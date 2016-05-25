package com.usp.icmc.labes.fsm;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import com.usp.icmc.ssc5888.Maze;
import com.usp.icmc.ssc5888.TopologicalMap;

public class MazeUtils {

	private static MazeUtils instance;

	private MazeUtils() { }

	public static MazeUtils getInstance() {
		if(instance==null) instance = new MazeUtils();
		return instance;
	}

	public FsmModel extractTopologicalMap(Maze maze){

		FsmModel topoMap = new TopologicalMap();

		return topoMap;
	}

	public void saveMaze(File f, Maze m) throws IOException{

		BufferedWriter bw = new BufferedWriter(new FileWriter(f));

		StringBuffer sb = new StringBuffer();

		for (int y = m.getN(); y >= 1; y--) {
			for (int x = 1; x <= m.getN(); x++) {
				if (m.getNorth()[x][y]) sb.append("+--");
				else sb.append("+  ");
			}
			sb.append("+\n");
			for (int x = 1; x <= m.getN(); x++) {
				if (m.getWest() [x][y]) sb.append("|");
				else  sb.append(" ");
				sb.append("  ");
//				if (m.getEast() [x][y]) sb.append("|");
//				else  sb.append(" ");
			}
			sb.append("|\n");
			bw.write(sb.toString());
			sb.delete(0, sb.length());
		}
		for (int x = 1; x <= m.getN(); x++) {
			if (m.getSouth()[x][1]) sb.append("+--");
			else sb.append("+  ");
		}
		sb.append("+\n");
		bw.write(sb.toString());
		sb.delete(0, sb.length());

		bw.close();
	}
}
