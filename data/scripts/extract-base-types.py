import re

fr = file("/work/research/actup-production/data/types.txt", 'r')
fw = file("/work/research/actup-production/data/base-types.txt", 'w')

lines = fr.readlines()
wlines = set()

for line in lines:
	nonpotentials = line.split("/")
	potentials = []
	for p in nonpotentials:
		potentials.extend(p.split("\\"))
	for p in potentials:
		#p = re.sub("\[[a-z]+\]", "", p)
		#p = re.sub("\(", "", p)
		#p = re.sub("\)", "", p)
		wlines.add(p)
fw.writelines(wlines)	
				
