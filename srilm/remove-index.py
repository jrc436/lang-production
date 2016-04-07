fr = file("sentences.csv", 'r')
fw = file("sentences.txt", 'w')
lst = []
for line in fr.readlines():
	lineparts = line.split(",")
	linepart1 = ",".join(lineparts[1:len(lineparts)])
	if "rawToken" not in linepart1:
		fw.write(linepart1[1:-2]+"\n")
