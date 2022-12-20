#!/bin/bash

declare -i total

let total=0

for i in $(seq 1 30)
do
  score=$(cat results/output-$i.txt)
  total=$(( total + ( i * score ) ))
done

echo $total
