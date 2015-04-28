s=".lm"
for i in "tset"/*; do 
	bin/macosx/ngram-count -order 4 -text $i -lm $i$s -wbdiscount
done
