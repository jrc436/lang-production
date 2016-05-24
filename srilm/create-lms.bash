s=".lm"
i="bnc.txt"
#for i in "tset"/*; do 
bin/i686-m64/ngram-count -unk -order 5 -text $i -lm $i$s -wbdiscount
#done
