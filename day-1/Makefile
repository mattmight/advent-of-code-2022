

part1: input.txt calorie-count.awk
	awk -f calorie-count.awk < input.txt | sort -rn | head -1


part2: input.txt calorie-count.awk
	awk -f calorie-count.awk < input.txt | sort -rn | head -3 | awk -f sum.awk

