CORPUS_BP="/work/research/ap-largefiles/data/trainingsubsets"
LM_OUT_BP="/work/research/ap-largefiles/lm/swbd/"

s="000"
e="100"

for f in $CORPUS_BP/*; do
	
	fn="${f##*/}"
	fname="${fn%.*}"
	fileout="$LM_OUT_BP/${fname}.lm"
	fnum="${fname##*-}"	
	if [ $fnum -lt $s ] || [ $fnum -gt $e ]
		then continue 
	fi
	ngram-count -text $f -lm $fileout -order 4 -interpolate -wbdiscount
	echo $fnum	
done
