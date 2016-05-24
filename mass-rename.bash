s="lp-experiment"
input="experiment-models/"
#s="experiment"
j=0
k=".jactr"
for i in "experiment-models"/*; do 
	mv $i $input$s`printf %02d $j`$k
	((j++))
done
