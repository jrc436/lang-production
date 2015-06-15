package runconfig;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.BlockingQueue;

import optimization.VariableSet;
import realize.Realization;


//if it wasn't obvious, this class is not thread safe and should never be used across threads. It's point is to monitor the progress
//of individual threads for reporting to the person running the experiment
//in theory, it can replace the logger, but the logger is more designed as a catchall than anything else
public class ThreadState {
	private int lockNum; //the lock number that this thread currently has, or -1 if not on a lock
	private int fileNum; //the file number that this thread currently has, or -1 if not on a lock/file
	private final Set<Integer> locksFinished; //the locks this thread has already finished
	private Set<Integer> filesFinished; //the files already completed on this lock
	private final int numLocks; //the total number of locks this has to go through
	private int numFiles; //the total number of files remaining on this lock
	private String lockStartTime;
	private String fileStartTime;
	private String iterStartTime;
	private final int maxiter;
	private int currentIter;
	private final Saver s;
	public ThreadState(BlockingQueue<Message> result, IOSettings io, int numLocks, int maxiter) {
		this.lockNum = -1;
		this.fileNum = -1;
		this.locksFinished = new HashSet<Integer>();
		this.filesFinished = new HashSet<Integer>();
		this.numLocks = numLocks;
		this.numFiles = 0;
		this.lockStartTime = "nil";
		this.fileStartTime = "nil";
		this.iterStartTime = Client.getFormattedDate();
		this.maxiter = maxiter;
		this.currentIter = 1;
		s = new Saver(result, io);
	}
	public void completeLock(int lock) {
		locksFinished.add(lock);
		this.lockNum = -1;
		filesFinished.clear();
	}
	public void completeFile(int files) {
		filesFinished.add(files);
		this.fileNum = -1;
	}
	public void handleSaving(String expName, List<Realization> realizations, List<Integer> files, VariableSet vars, long time) {
		s.handleSaving(expName, realizations, files, vars, time);
	}
	public void requestSave() {
		s.userSaving();
	}
	public void startFile(int file) {
		this.fileNum = file;
		this.fileStartTime = Client.getFormattedDate();
	}
	public void startLock(int lock, int numFiles) {
		this.lockNum = lock;
		this.numFiles = numFiles;
		this.lockStartTime = Client.getFormattedDate();
	}
	public void completeIter() {
		locksFinished.clear();
		currentIter++;
		iterStartTime = Client.getFormattedDate();
	}
	
	//should only be called from I/O threads
	public String status() {
		if (locksFinished.size() == numLocks && currentIter == maxiter) {
			return "Thread has completed all locks. Waiting for other threads.";
		}
		else if (lockNum == -1) {
			return "Thread is attempting to acquire a lock";
		}
		else if (fileNum == -1) {
			return "Thread is in between files on lock: "+lockNum;
		}
		return "Working on iter: "+currentIter+" since "+ iterStartTime+"; lock: "+lockNum+" since "+lockStartTime+"; file: "+fileNum+" since "+fileStartTime;
	}
	public String progress() {
		String iterprefix = "Iteration: "+currentIter+ " / "+maxiter+"; ";
		String locksuffix = "Locks completed: "+locksFinished.size()+" / "+numLocks+" { ";
		for (int l : locksFinished) {
			locksuffix += l+",";
		}
		locksuffix = locksuffix.substring(0, locksuffix.length()-1)+" }";
		if (lockNum == -1) {
			return "Thread is attempting to acquire a lock. "+locksuffix;
		}
		String lockprefix = "Current lock: "+lockNum+"; Files completed "+filesFinished.size()+" / "+numFiles+" { ";
		for (int f : filesFinished) {
			lockprefix += f+",";
		}
		lockprefix = lockprefix.substring(0, lockprefix.length()-1)+" }; ";
		return iterprefix+lockprefix + locksuffix;
	}
}
