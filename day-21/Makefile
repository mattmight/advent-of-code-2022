part1: part1.bc
	bc -f part1.bc

part2: part2.bc
	bc -f part2.bc -f solve.bc

part2.bc:
	sed -E -f part2.sed < input.txt > part2.bc
	echo 'scale = 0;' >> part2.bc

part1.bc: 
	sed -E -f part1.sed < input.txt > part1.bc
	echo 'scale = 0; print (root()); print "\\n";' >> part1.bc

clean:
	rm -fv part1.bc part2.bc
