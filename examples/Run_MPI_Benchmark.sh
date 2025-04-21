#!/bin/bash

for ((i = 0 ; i < $1 ; i++)); do
         echo "Running program iteration $i"
	 time mpirun --oversubscribe -np 7 $2 > Result_$2_$i.txt
done
