f = file("/work/research/actup-production/data/swbd-rawtext-full.txt", 'r')
lines = f.readlines()

cutoff = 15
count = 0

for line in lines:
	if len(line.split(" ")) > cutoff:
		count = count + 1
print count
