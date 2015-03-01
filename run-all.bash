#!/bin/bash
cd openccg/actup-realizer/bin
n="$(uname -a)"
if [[ $n =~ .*ying.* ]]
then
	mem="3g"
	client="Eclipse"
	size="sample"
	bp="/work/research"
else
	mem="20g"
	client="Terminal"
	size="full"
	bp="/Users/jrc/Public/jrc-research/"
java -Xmx$mem opennlp.ccg.Client${client}.java -actr
java -Xmx$mem opennlp.ccg.Client${client}.java -notactr
#now we have output in ap-largefiles/data/swbd/SOMETHING-out-SOMETHING
#next step is to make sure it's in the correct format for rouge!
cd ../../../../ap-largefiles/scripts
filename1="swbd-rawtext-${size}-out-actr.txt"
filename2="swbd-rawtext-${size}-out-std.txt"
#make sure the subdirectories "gold" and "realizations" exist
python convert-realization-format.py $bp $filename1
python convert-realization-format.py $bp $filename2
cd ..
mv data/gold/swbd-rawtext-${size}-out-actr-gold.txt ../actup-production/rouge/actup/gold/actr-goals.spl
mv data/gold/swbd-rawtext-${size}-out-std-gold.txt ../actup-production/rouge/actup/gold/std-goals.spl
mv data/realizations/swbd-rawtext-${size}-out-actr-realize.txt ../actup-production/rouge/realizations/actup/actr-out.spl
mv data/realizations/swbd-rawtext-${size}-out-std-realize.txt ../actup-production/rouge/realizations/actup/std-out.spl

cd ../actup-production/rouge
./ROUGE-1.5.5.pl -a actup/config.xml > output.txt 
