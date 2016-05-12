package com.usp.icmc.labes.fsm;

public class DrawUtils {

	private static DrawUtils instance;

	private DrawUtils() { }

	public static DrawUtils getInstance() {
		if(instance==null) instance = new DrawUtils();
		return instance;
	}

	boolean showWindow = true;
	
	public void setShowWindow(boolean showWindow) {
		this.showWindow = showWindow;
	}
	
	public boolean getShowWindow() {
		return this.showWindow;
	}

}
