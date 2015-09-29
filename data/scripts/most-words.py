f = file("/work/research/actup-production/data/swbd-rawtext-full.txt", 'r')
lines = f.readlines()

mostWords = 0
biggestLine = ""
for line in lines:
	numWords = len(line.split(" "))
	if numWords > mostWords:
		mostWords = numWords
		biggestLine = line
print mostWords
print biggestLine
