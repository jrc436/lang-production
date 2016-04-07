s=".lm"
i="sentences.txt"
#for i in "tset"/*; do 
	bin/i686-m64/ngram-count -order 2 -text $i -lm $i$s -wbdiscount
#done
