import re

f = open("grammar/lexicon.xml", 'r')
#ft = f.read()
#regex1 = re.compile("[\s]*<slash[\D\d]{,25}/>$\n[\s]*<atomcat", re.MULTILINE)
#regex2 = re.compile("[\s]*<slash[\D\d]{,25}/>$\n[\s]*<complexcat", re.MULTILINE)
#regex = re.compile("[\s]*<slash[\D\d]{,25}/>$\n^[\s]*<[a-z]*", re.MULTILINE)
slashregex = re.compile("slash")
fsregex = re.compile("fs")

lines = f.readlines();

fw = open("debug_output.txt", 'w')
print(len(lines))
for i in range(len(lines)):
	if slashregex.search(lines[i]) is not None:
		if i >= len(lines)-1:
			print("SLASH END POSITION!!")
			break;
		elif "<atomcat" not in lines[i+1] and "<complexcat" not in lines[i+1] and "<dollar" not in lines[i+1]:
			fw.write(str(i)+"(slash):"+lines[i+1])

	elif fsregex.search(lines[i]) is not None:
		if i >= len(lines):
			print("FS END POSITION!!")
			break;
		elif "<atomcat" not in lines[i-1] and "<complexcat" not in lines[i-1] and "<dollar" not in lines[i-1]:
			fw.write(str(i)+"(fs):"+lines[i-1])
f.close()
fw.close()
 
