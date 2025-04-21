#!/bin/bash

mkdir Final_Results
touch Final_Results/Result.txt

for filename in *;
do sed "2q;d" $filename >> ./Final_Results/Result.txt;
done

cd ./Final_Results
sed -i 's/^.\{20\}//' Result.txt
sed -i "s/.\{2\}$//" Result.txt
echo "==============================******Average******=============================" >> Result.txt
average Result.txt >> Result.txt
