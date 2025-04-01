#!/bin/bash

for ((i = 0 ; i < $1 ; i++)); do
         echo "Running program"
	./$2 > Result_$2_$i.txt
done
