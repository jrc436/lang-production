import re

fr = file("/work/research/actup-production/data/types.txt", "r")
fw = file("/work/research/actup-production/data/subtypes.txt", "w")

lines = fr.readlines()
writelines = set()
for line in lines:
	possiblematches = re.findall("\[[a-z]+\]", line)
	for match in possiblematches:
		if match.count("[") == 1:
			stripbrackets = match[1:-1]
			writelines.add(stripbrackets+",\n")
fw.writelines(writelines)		
			
