i = 650

conf = file("config.xml", 'w')
conf.write('<ROUGE-EVAL version="1.0">\n')

asID = "actr-swm1"
awID = "actr-wsj"
ssID = "std-swm1"
swID = "std-wsj"
goals = "gold"

for line in range(i):
        lineF = "%03d"%line
	
	conf.write('<EVAL ID="SWBD-'+lineF+'">\n')
        conf.write("<PEER-ROOT>\n")
        conf.write("actup/realizations\n")
        conf.write("</PEER-ROOT>\n")
        conf.write("<MODEL-ROOT>\n")
	conf.write("actup/gold\n")
        conf.write("</MODEL-ROOT>\n")
        conf.write('<INPUT-FORMAT TYPE="SPL">\n')
        conf.write("</INPUT-FORMAT>\n")
        conf.write("<PEERS>\n")
        
	#actr-swbd, actr-wsj, std-swbd, std-wsj
	conf.write('<P ID="'+asID+'">'+asID+'-'+lineF+".spl"+'</P>\n')
        conf.write('<P ID="'+awID+'">'+awID+'-'+lineF+".spl"+'</P>\n')
	#conf.write('<P ID="'+ssID+'">'+ssID+'-'+lineF+".spl"+'</P>\n')
	#conf.write('<P ID="'+swID+'">'+swID+'-'+lineF+".spl"+'</P>\n')
	conf.write("</PEERS>\n")
        conf.write("<MODELS>\n")
        conf.write('<M ID="'+asID+'">'+goals+'-'+lineF+'.spl</M>\n')
	conf.write('<M ID="'+awID+'">'+goals+'-'+lineF+'.spl</M>\n')
	#conf.write('<M ID="'+ssID+'">'+goals+'-'+lineF+'.spl</M>\n')
	#conf.write('<M ID="'+swID+'">'+goals+'-'+lineF+'.spl</M>\n')
	conf.write("</MODELS>\n")
        conf.write("</EVAL>\n")

conf.write("</ROUGE-EVAL>")



