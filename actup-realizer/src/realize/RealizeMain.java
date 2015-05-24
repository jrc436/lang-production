///////////////////////////////////////////////////////////////////////////////
// Copyright (C) 2003-7 University of Edinburgh, Michael White
// 
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
// 
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
//////////////////////////////////////////////////////////////////////////////

package realize;

import grammar.Grammar;

import java.util.Queue;
import java.util.Set;

import ngrams.AbstractStandardNgramModel;
import pruning.NBestPruningStrategy;
import pruning.PruningStrategy;
import runconfig.InputHandler;
import runconfig.InputStruct;
import runconfig.LMHandler;
import runconfig.RealizationSettings;

public class RealizeMain
{
	public static final int NO_LOCK_AVAIL = -1;
	public static final int NO_LOCK_REMAINING = -2;
	private final PruningStrategy ps;
	private final RealizationSettings rs;
	private final Grammar g;
	private final InputHandler inh;
	private final LMHandler lm;

	//should only need one realizemain, generally, as it is generally threadsafe
	//with the locking mechanisms in valleyclimber
	public RealizeMain(RealizationSettings rs, Grammar grammar, InputHandler inh, LMHandler lm) {
		this.rs = rs;
		this.ps = new NBestPruningStrategy(rs.getEdgePruningValue());
		this.g = grammar;
		this.inh = inh;
		this.lm = lm;
	}
	
	public synchronized int attemptAcquireLock(Set<Integer> alreadyHad, String runName, Queue<String> log, int timeoutNum) {
		int lock = RealizeMain.NO_LOCK_AVAIL;
		int iter = 0;		
		while (lock == RealizeMain.NO_LOCK_AVAIL) {
			log.offer(runName + " is attempting to acquire a lock");
			lock = lm.attemptAcquireLock(alreadyHad);
			iter++;
			while (iter > timeoutNum) {
				try {
					wait();
				} catch (InterruptedException e) {
					iter = 0;
				}
			}
		}
		return lock;
	}
	public synchronized AbstractStandardNgramModel getScorer(int lock, double[] vars) {
		return lm.getFreshModel(lock, vars);
	}
	public synchronized Queue<InputStruct[]> getInput(int lock) {
		return inh.getCurrentLockQueue(lock);
	}
	public synchronized void releaseLock(int lock) {
		lm.releaseLock(lock);
		notify();
	}
	public Set<Integer> getRunFiles() {
		return inh.getFiles();
	}
	
	public Realizer createNewRealizer() {
		return new Realizer(rs, ps, g);
	}
	
	//each one of these calls needs its own realizer to make it threadsafe.
	public Realization[] realize(AbstractStandardNgramModel ngramScorer, Realizer realizer, InputStruct[] items, Queue<String> log, String expname) {              

		Realization[] r = new Realization[items.length];
		
		for (int i = 0; i < items.length; i++) {
			Chart chart = realizer.realize(items[i].getLF(), ngramScorer);
			r[i] = chart.getBestRealization(items[i].getGoal());
			log.offer(expname+","+"l"+i+":"+r[i].str);
			ngramScorer.updateAfterRealization(items[i].getGoal());
		}
		return r;
	}
}
