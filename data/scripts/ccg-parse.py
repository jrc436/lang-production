#!/usr/bin/python/

f = open('/work/research/actup-production/switchboard_orig.ccg', 'r')

lines = f.readlines()

#we need to parse this in terms of "statements", which are surrounded by {} 

linesSoFar = 0
lineInd = []
bigIter = 0
for line in lines:
	if linesSoFar > 413332 and "NEW SENTENCE" in line:
		linesSoFar = 0
		bigIter = bigIter + 1
		f2 = open('/work/research/actup-production/swbd'+str(bigIter)+'.ccg', 'w')
		f2.writelines(lineInd)
		f2.close()
		lineInd = []
	lineInd.append(line)
f.close()
