package util.sys;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;

public abstract class Executable implements Runnable, Callable<String> {
	private final int threadnum;
	private final BlockingQueue<String> log;
	public Executable(int threadnum, BlockingQueue<String> log) {
		this.log = log;
		this.threadnum = threadnum;
	}
	public String call() {
		run();
		return "";
	}
	protected int getNum() {
		return threadnum;
	}
	
	protected void logMessage(String msg) {
		log.offer(msg);
	}
	public abstract void run();
}
