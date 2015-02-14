import re
from sets import Set

f = open("/home/jrc/actup-production/openccg/grammars/switchboard/lexicon.xml", 'r')
#ft = f.read()
regex1 = re.compile("[\s]*<slash[\D\d]{,25}/>$\n[\s]*<atomcat", re.MULTILINE)
regex2 = re.compile("[\s]*<slash[\D\d]{,25}/>$\n[\s]*<complexcat", re.MULTILINE)
regex = re.compile("[\s]*<slash[\D\d]{,25}/>$\n^[\s]*<[a-z]*", re.MULTILINE)
capregex = re.compile("slash")
lines = f.readlines();

fw = open("/home/jrc/actup-production/openccg/grammars/switchboard/pyout.txt", 'w')
print(len(lines))
for i in range(len(lines)):
	if capregex.search(lines[i]) is not None:
		if i >= len(lines):
 			fw.write("END POSITION!!")
			print("END POSITION!!")
		elif "<atomcat" not in lines[i+1] and "<complexcat" not in lines[i+1] and "<dollar" not in lines[i+1]:
			fw.write(str(i)+":"+lines[i+1])
#output = regex.findall(ft)
#output1 = regex1.findall(ft)
#output2 = regex2.findall(ft)
#outputcap = capregex.findall(ft)
f.close()
fw.close()
 
