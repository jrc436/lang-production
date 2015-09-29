f = file("/work/research/actup-production/data/words.dsv", 'r')
lines = f.readlines()

curMax = 0
curWinner = ""
for line in lines:
	thisCount = line.count(":-:")
	if thisCount > curMax:
		curMax = thisCount
		curWinner = line
print curMax
print curWinner
