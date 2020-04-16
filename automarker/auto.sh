#!/bin/bash

executable="./myinterpreter"

total=0
totalpassed=0
for i in 1 2 3 4 5
do
	echo 
	echo "*****************"
	echo "*     pr $i     *"
	echo "*****************"	
	prog="pr$i/pr$i.spl"
	passed=0
	in="pr$i"
	for X in $in/inputs/*.txt
	do
		j=${X#pr$i/inputs/input}
		j=${j%.txt}
		total=$((total+1))
		echo 
		echo "  ******************************"
		echo "  *          test $j           *"
		echo "  ******************************"
		in="pr$i/inputs/input"
		in+=$j
		in+=".txt"
		out="pr$i/outputs/out$j.txt"

		echo "  Input:"
		echo "  ------"
		cat $in | sed "s/^/  /"
		echo
		
		$executable $prog < $in > $out 2>/dev/null

		if [ "$?" = 124 ]; then
			echo -e "  TEST FAILED -- TIMEOUT"
		else
			outofficial="pr$i/expected/"
			outofficial+="exp" 
			outofficial+="$j.txt"

			diff -wB $out $outofficial > /dev/null
			if [ "$?" = 0 ]; then
				passed=$((passed+1))
				echo -e "  TEST PASSED" 
			else
				echo -e "  TEST FAILED" 
				echo 
				echo "  Your output:"
				echo "  ------------"
				cat $out | sed "s/^/  /"
				echo
				echo "  Correct output:"
				echo "  ---------------"
				cat $outofficial | sed "s/^/  /"
				echo
			fi
		fi
	done
	echo 
	echo "$passed tests passed for pr$i"
	totalpassed=$((totalpassed+passed))
done
echo "$totalpassed tests passed out of a total of $total"